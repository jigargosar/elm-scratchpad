module TIS100.Sim exposing
    ( Sim
    , init
    , step
    , view
    )

import Dict exposing (Dict)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as State exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import Utils exposing (..)


type alias Addr =
    ( Int, Int )


type Node
    = InputNode String InputNode
    | OutputNode String OutputNode
    | ExeNode ExeNode


type alias Store =
    Dict Addr Node


type alias ReadBlockedStore =
    Dict Addr ReadBlockedNode


type alias ReadBlockedNode =
    ( Node, Num -> Node )


type alias WriteBlockedStore =
    Dict Addr WriteBlockedNode


type alias WriteBlockedNode =
    ( Node, Num, () -> Node )


type alias NodeEntry =
    ( Addr, Node )


type alias Sim =
    { store : Store
    , cycle : Int
    }


initInputNode : String -> Int -> Node
initInputNode title hi =
    InputNode title (InputNode.fromList (Num.range 1 hi))


initOutputNode : String -> Int -> Node
initOutputNode title expected =
    OutputNode title (OutputNode.fromExpected expected)


initExe : Node
initExe =
    ExeNode ExeNode.init


init : Sim
init =
    let
        col1 : Store
        col1 =
            [ initInputNode "IN.A" 3, initExe, initExe, initOutputNode "OUT.A" 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 0))
                |> Dict.fromList

        col2 : Store
        col2 =
            [ initInputNode "IN.B" 2, initExe, initExe, initOutputNode "OUT.B" 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 1))
                |> Dict.fromList

        col3 : Store
        col3 =
            [ initInputNode "IN.C" 3, initExe, initExe, initOutputNode "OUT.C" 2 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 2))
                |> Dict.fromList

        nodeStore =
            Dict.union col1 col2 |> Dict.union col3
    in
    { store = nodeStore, cycle = 0 }


type Port
    = Port Addr Dir PortValue


type PortValue
    = Empty
    | Num Num
    | Query


type Dir
    = Down


viewPort : Port -> Html msg
viewPort (Port addr Down mbNum) =
    div
        [ gridAreaFromPortDown addr
        , displayGrid
        , gridTemplateColumns "1fr 1fr"
        , pointerEvents "all"
        ]
        [ div [] []
        , div [ dGrid, style "place-content" "center" ]
            [ viewDownArrow mbNum
            ]
        ]


gridAreaFromPortDown : Addr -> Attribute msg
gridAreaFromPortDown ( x, y ) =
    gridAreaXY
        ( x * 2
        , if y == 0 then
            0

          else if y == maxY then
            (y * 2) - 2

          else
            y * 2
        )


step : Sim -> Sim
step sim =
    { sim
        | store = stepNodes sim.store
        , cycle = sim.cycle + 1
    }


stepNodes : Store -> Store
stepNodes ns =
    Dict.foldl stepNode emptyAcc ns
        |> resolveAllReadBlocked
        |> resolveAllWriteBlocked


stepNode : Addr -> Node -> Acc -> Acc
stepNode addr node =
    case nodeState node of
        State.Write num _ writeResolver ->
            addToWriteBlocked addr ( node, num, writeResolver )

        State.Done ->
            addToCompleted addr node

        State.Read readResolver ->
            addToReadBlocked addr ( node, readResolver )

        State.Run runResolver ->
            resolveAfterRun addr (runResolver ())


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        InputNode title inputNode ->
            InputNode.state inputNode |> State.map (InputNode title)

        OutputNode title outputNode ->
            OutputNode.state outputNode |> State.map (OutputNode title)

        ExeNode exeNode ->
            ExeNode.state exeNode |> State.map ExeNode


resolveAfterRun : Addr -> Node -> Acc -> Acc
resolveAfterRun addr node =
    case nodeState node of
        State.Read resolver ->
            addToReadBlocked addr ( node, resolver )

        _ ->
            addToCompleted addr node


resolveAllReadBlocked : Acc -> Acc
resolveAllReadBlocked acc =
    Dict.foldl resolveReadBlocked { acc | readBlocked = Dict.empty } acc.readBlocked


resolveReadBlocked :
    Addr
    -> ReadBlockedNode
    -> WriteBlockedAcc a
    -> WriteBlockedAcc a
resolveReadBlocked addr ( node, readResolver ) acc =
    case readFromAddrAndUnblock (addrUp addr) acc of
        Just ( num, acc2 ) ->
            addToCompleted addr (readResolver num) acc2

        Nothing ->
            addToCompleted addr node acc


addrUp : Addr -> Addr
addrUp ( x, y ) =
    ( x, y - 1 )


readFromAddrAndUnblock :
    Addr
    -> WriteBlockedAcc a
    -> Maybe ( Num, WriteBlockedAcc a )
readFromAddrAndUnblock addr acc =
    case Dict.get addr acc.writeBlocked of
        Just ( _, num, resolver ) ->
            Just
                ( num
                , addToCompleted addr
                    (resolver ())
                    { acc
                        | writeBlocked = Dict.remove addr acc.writeBlocked
                    }
                )

        Nothing ->
            Nothing


resolveAllWriteBlocked : WriteBlockedAcc a -> Store
resolveAllWriteBlocked acc =
    Dict.foldl (\na ( n, _, _ ) -> Dict.insert na n) acc.completed acc.writeBlocked


type alias Acc =
    { readBlocked : ReadBlockedStore
    , writeBlocked : WriteBlockedStore
    , completed : Store
    }


type alias WriteBlockedAcc a =
    { a
        | writeBlocked : WriteBlockedStore
        , completed : Store
    }


emptyAcc : Acc
emptyAcc =
    { readBlocked = Dict.empty
    , writeBlocked = Dict.empty
    , completed = Dict.empty
    }


addToWriteBlocked :
    Addr
    -> WriteBlockedNode
    -> { a | writeBlocked : WriteBlockedStore }
    -> { a | writeBlocked : WriteBlockedStore }
addToWriteBlocked na n acc =
    { acc | writeBlocked = Dict.insert na n acc.writeBlocked }


addToCompleted :
    Addr
    -> Node
    -> { a | completed : Store }
    -> { a | completed : Store }
addToCompleted na n acc =
    { acc | completed = Dict.insert na n acc.completed }


addToReadBlocked :
    Addr
    -> ReadBlockedNode
    -> { a | readBlocked : ReadBlockedStore }
    -> { a | readBlocked : ReadBlockedStore }
addToReadBlocked na n acc =
    { acc | readBlocked = Dict.insert na n acc.readBlocked }


view : Sim -> Html msg
view sim =
    fCol [ h100, pa "10px", fontSize "10px", bold, ffMonospace ]
        [ div [] [ text "Cycle: ", text (fromInt sim.cycle) ]
        , viewGrid sim
        ]


viewGrid : Sim -> Html msg
viewGrid sim =
    div
        [ displayGrid
        , gridTemplateRows
            ("repeat(" ++ fromInt (maxY - 1) ++ ", 1fr 2fr) 1fr")
        , gridTemplateColumns "repeat(2, 2fr 1fr) 2fr"
        ]
        (viewNodes sim ++ viewPorts sim)


viewPorts : Sim -> List (Html msg)
viewPorts sim =
    getPorts sim |> List.map viewPort


getPorts : Sim -> List Port
getPorts sim =
    foldlEntries (getNodePorts >> List.append) [] sim.store


getNodePorts : NodeEntry -> List Port
getNodePorts ( addr, node ) =
    --case node of
    --    InputNode _ input ->
    --        [ Port addr Down (InputNode.numForView input) ]
    --
    --    OutputNode _ _ ->
    --        [ Port addr Down Nothing ]
    --
    --    ExeNode _ ->
    case nodeState node of
        State.Run _ ->
            []

        State.Read _ ->
            []

        State.Write num _ _ ->
            [ Port addr Down (Num num) ]

        State.Done ->
            []


viewNodes : Sim -> List (Html msg)
viewNodes sim =
    Dict.toList sim.store |> List.map viewNode



--noinspection ElmUnusedSymbol


maxX =
    3


maxY =
    3


nodeAddrToGridArea : Addr -> Attribute msg
nodeAddrToGridArea ( x, y ) =
    gridAreaXY
        ( x * 2
        , if y == 0 then
            0

          else if y == maxY then
            (y * 2) - 2

          else
            (y * 2) - 1
        )


viewNode : NodeEntry -> Html msg
viewNode ( addr, node ) =
    case node of
        InputNode title _ ->
            div
                [ nodeAddrToGridArea addr
                , dGrid
                , gridTemplateColumns "1fr 1fr"
                , placeItemsCenter
                ]
                [ div [ tac, fg (grayN 0.7) ]
                    [ div [] [ text title ]
                    , div [] [ text "(IDLE 0%)" ]
                    ]
                ]

        OutputNode title _ ->
            div
                [ nodeAddrToGridArea addr
                , dGrid
                , gridTemplateColumns "1fr 1fr"
                , placeItemsCenter
                ]
                [ div [ tac, fg (grayN 0.7) ] [ text title ]
                ]

        ExeNode _ ->
            div
                [ sOutline "1px solid white"
                , pa "10px"
                , sMinWidth "18ch"
                , sMinHeight "18ch"
                , nodeAddrToGridArea addr
                , tac
                ]
                [ text "MODE:", text (nodeBlockMode node) ]


viewDownArrow : PortValue -> Html msg
viewDownArrow mbNum =
    fRow []
        [ div [ fontSize "2em", fontWeight "100" ] [ text "⇓" ]
        , case mbNum of
            Empty ->
                noView

            Num num ->
                div [] [ text <| Num.toString num ]

            Query ->
                div [] [ text "?" ]
        ]


nodeBlockMode : Node -> String
nodeBlockMode node =
    case nodeState node of
        State.Run _ ->
            "RUN"

        State.Read _ ->
            "READ"

        State.Write _ _ _ ->
            "WRITE"

        State.Done ->
            "IDLE"
