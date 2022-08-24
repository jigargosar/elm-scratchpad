module TIS100.Sim exposing
    ( Sim
    , init
    , step
    , view
    )

import Dict exposing (Dict)
import Html.Attributes as HA
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as State exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import Utils exposing (..)


type alias Addr =
    ( Int, Int )


type Node
    = InputNode InputNode
    | OutputNode OutputNode
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


initInputNode : Int -> Node
initInputNode hi =
    InputNode (InputNode.fromList (Num.range 1 hi))


initOutputNode : Int -> Node
initOutputNode expected =
    OutputNode (OutputNode.fromExpected expected)


initExe : Node
initExe =
    ExeNode ExeNode.init


init : Sim
init =
    let
        col1 : Store
        col1 =
            [ initInputNode 3, initExe, initOutputNode 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 0))
                |> Dict.fromList

        col2 : Store
        col2 =
            [ initInputNode 2, initExe, initOutputNode 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 1))
                |> Dict.fromList

        col3 : Store
        col3 =
            [ initInputNode 3, initExe, initOutputNode 2 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 2))
                |> Dict.fromList

        nodeStore =
            Dict.union col1 col2 |> Dict.union col3
    in
    { store = nodeStore, cycle = 0 }


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
        State.Write num writeResolver ->
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
        InputNode inputNode ->
            InputNode.state inputNode |> State.map InputNode

        OutputNode outputNode ->
            OutputNode.state outputNode |> State.map OutputNode

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
    div [ pa "10px", fontSize "10px", bold, ffMonospace ]
        [ div [] [ text "Cycle: ", text (fromInt sim.cycle) ]
        , div [ dGrid ]
            (Dict.toList sim.store |> List.map viewNode)
        ]


viewNode : NodeEntry -> Html msg
viewNode (( addr, node ) as entry) =
    div [ pa "10px", gridAreaXY addr, HA.title (Debug.toString entry) ]
        [ noView
        , text (nodeToString node) |> always noView
        , viewNodeHelp entry
        , text (Debug.toString (nodeState node)) |> always noView
        , text (Debug.toString entry) |> always noView
        ]


viewNodeHelp : NodeEntry -> Html msg
viewNodeHelp ( addr, node ) =
    case node of
        InputNode input ->
            div [ dGrid, gridAutoFlowColumn ]
                [ div [ tac ]
                    [ div [] [ text "IN.A" ]
                    , div [] [ text "(IDLE 0%)" ]
                    ]
                , div [ fontSize "2em", fontWeight "100" ] [ text "⇓" ]
                ]

        OutputNode output ->
            div [ tac, positionRelative ]
                [ div [] [ text "OUT.A" ]
                , div
                    [ positionAbsolute
                    , w100
                    , bottom100

                    --, styleLineHeight "0.5"
                    ]
                    [ text "⇑" |> always noView
                    , text "⇓"
                    ]
                ]

        ExeNode exe ->
            div [ tac ] [ text (nodeBlockMode node) ]


nodeBlockMode : Node -> String
nodeBlockMode node =
    case nodeState node of
        State.Run function ->
            "N/A"

        State.Read function ->
            "READ"

        State.Write num function ->
            "WRITE"

        State.Done ->
            "N/A"


nodeToString : Node -> String
nodeToString node =
    case node of
        InputNode inputNode ->
            Debug.toString inputNode

        OutputNode outputNode ->
            Debug.toString outputNode

        ExeNode exeNode ->
            Debug.toString exeNode
