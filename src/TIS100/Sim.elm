module TIS100.Sim exposing
    ( Sim
    , sampleSim
    , step
    , view
    )

import Dict exposing (Dict)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as S exposing (NodeState)
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
    ( Node, Dir4, Num -> Node )


type alias WriteBlockedStore =
    Dict Addr WriteBlockedNode


type alias WriteBlockedNode =
    { node : Node, num : Num, dir : Dir4, cont : () -> Node }


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


init :
    List ( Int, String, InputNode )
    -> List ( Int, String, OutputNode )
    -> List ( Addr, ExeNode )
    -> Sim
init is os es =
    let
        store =
            Dict.empty
                |> withInputs is
                |> withOutputs os
                |> withExecutables es
    in
    { store = store, cycle = 0 }


withInputs : List ( Int, String, InputNode ) -> Store -> Store
withInputs list store =
    List.foldl
        (\( x, title, input ) -> Dict.insert ( x, 0 ) (InputNode title input))
        store
        list


withOutputs : List ( Int, String, OutputNode ) -> Store -> Store
withOutputs list store =
    List.foldl
        (\( x, title, out ) -> Dict.insert ( x, maxY ) (OutputNode title out))
        store
        list


withExecutables : List ( Addr, ExeNode ) -> Store -> Store
withExecutables list store =
    List.foldl
        (\( addr, exe ) -> Dict.insert addr (ExeNode exe))
        store
        list


sampleSim : Sim
sampleSim =
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

        is =
            [ ( 0, "IN.A", InputNode.fromList (Num.range 1 3) ) ]

        os =
            [ ( 0, "OUT.A", OutputNode.fromExpected 3 ) ]

        es =
            [ ( ( 0, 1 ), ExeNode.init )
            , ( ( 0, 2 ), ExeNode.init )
            , ( ( 0, 3 ), ExeNode.init )
            ]

        sim : Sim
        sim =
            init is os es
    in
    { store = nodeStore, cycle = 0 }
        |> always sim


type Port
    = Port PortId PortValue


type PortValue
    = Empty
    | Num Num
    | Queried


updatePortValueOnQuery : PortValue -> PortValue
updatePortValueOnQuery portValue =
    case portValue of
        Empty ->
            Queried

        _ ->
            portValue


viewPortValueText : PortValue -> Html msg
viewPortValueText =
    let
        toString portValue =
            case portValue of
                Empty ->
                    ""

                Num num ->
                    Num.toString num

                Queried ->
                    "?"
    in
    toString >> text


type PortId
    = PortId Addr Dir4 PortKey


initPortId : Addr -> IOIntent -> Maybe PortId
initPortId addr ioIntent =
    case ioIntent of
        Read dir ->
            initWritePortId (moveInDir4 dir addr) (oppositeDir4 dir)

        Write dir ->
            initWritePortId addr dir


initWritePortId : Addr -> Dir4 -> Maybe PortId
initWritePortId (( fx, fy ) as from) dir =
    let
        (( tx, ty ) as to) =
            moveInDir4 dir from
    in
    if
        (fx < 0 || fx > maxX || fy < 0 || fy >= maxY)
            || (tx < 0 || tx > maxX || ty <= 0 || ty > maxY)
    then
        Nothing

    else
        Just <| PortId from dir ( from, to )


portKeyFromId : PortId -> PortKey
portKeyFromId (PortId _ _ key) =
    key


updatePortsDict :
    Addr
    -> IOIntent
    -> (PortValue -> PortValue)
    -> Ports
    -> Ports
updatePortsDict addr iOIntent fn ports =
    case initPortId addr iOIntent of
        Nothing ->
            ports

        Just portId ->
            Dict.update (portKeyFromId portId)
                (\mbPort ->
                    let
                        newPortValue =
                            case mbPort of
                                Nothing ->
                                    fn Empty

                                Just (Port _ portValue) ->
                                    fn portValue
                    in
                    Just (Port portId newPortValue)
                )
                ports


type alias PortKey =
    ( Addr, Addr )


type alias Ports =
    Dict PortKey Port


toPorts : Sim -> List Port
toPorts sim =
    foldlEntries addPortsFromNodeEntry
        Dict.empty
        sim.store
        |> Dict.values


addPortsFromNodeEntry : NodeEntry -> Ports -> Ports
addPortsFromNodeEntry entry ports =
    ports
        |> addPortsFromNodeIOIntents entry
        |> updatePortValuesFromNodeState entry


addPortsFromNodeIOIntents : NodeEntry -> Ports -> Ports
addPortsFromNodeIOIntents ( addr, node ) dict =
    nodeIoIntents node
        |> List.foldl
            (\ioIntent -> updatePortsDict addr ioIntent identity)
            dict


updatePortValuesFromNodeState : NodeEntry -> Ports -> Ports
updatePortValuesFromNodeState ( addr, node ) =
    if second addr == maxY then
        -- ignore updating read query for output node
        identity

    else
        case nodeState node of
            S.ReadyToRun _ ->
                identity

            S.ReadBlocked dir _ ->
                updatePortsDict addr (Read dir) updatePortValueOnQuery

            S.WriteBlocked num dir _ ->
                updatePortsDict addr (Write dir) (always (Num num))

            S.Done ->
                identity


nodeIoIntents : Node -> List IOIntent
nodeIoIntents node =
    case node of
        InputNode _ _ ->
            [ Write Down ]

        OutputNode _ _ ->
            [ Read Up ]

        ExeNode exe ->
            ExeNode.ioIntents exe


viewPort : Port -> Html msg
viewPort (Port (PortId addr dir _) portValue) =
    case dir of
        Up ->
            viewUpPortValue addr portValue

        Down ->
            viewDownPortValue addr portValue

        Left ->
            noView

        Right ->
            noView


viewDownPortValue : Addr -> PortValue -> Html msg
viewDownPortValue ( x, y ) portValue =
    gtCols 2
        [ gridAreaXY ( x * 2, y * 2 ), noPointerEvents ]
        [ fRow
            [ gridAreaXY ( 1, 0 )
            , allPointerEvents
            , itemsCenter
            , pl "1ch"
            , gap "1ch"
            ]
            [ viewArrow Down
            , viewPortValueText portValue
            ]
        ]


viewUpPortValue : Addr -> PortValue -> Html msg
viewUpPortValue ( x, y ) portValue =
    gtCols 2
        [ gridAreaXY ( x * 2, (y * 2) - 2 ), noPointerEvents ]
        [ fRow
            [ gridAreaXY ( 0, 0 )
            , allPointerEvents
            , itemsCenter
            , justifyContent "end"
            , pr "1ch"
            , gap "1ch"
            ]
            [ viewPortValueText portValue
            , viewArrow Up
            ]
        ]


viewArrow : Dir4 -> Html msg
viewArrow dir4 =
    span [ fontSize "2em", fontWeight "100" ] [ text (arrowDefault dir4) ]


step : Sim -> Sim
step sim =
    { sim
        | store =
            Dict.foldl stepNode emptyAcc sim.store
                |> resolveAllReadBlocked
                |> resolveAllWriteBlocked
        , cycle = sim.cycle + 1
    }


stepNode : Addr -> Node -> Acc -> Acc
stepNode addr node =
    case nodeState node of
        S.WriteBlocked num dir cont ->
            addToWriteBlocked addr node num dir cont

        S.Done ->
            addToCompleted addr node

        S.ReadBlocked dir cont ->
            addToReadBlocked addr node dir cont

        S.ReadyToRun cont ->
            resolveAfterRun addr (cont ())


resolveAfterRun : Addr -> Node -> Acc -> Acc
resolveAfterRun addr node =
    case nodeState node of
        S.ReadBlocked dir cont ->
            addToReadBlocked addr node dir cont

        _ ->
            addToCompleted addr node


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        InputNode title inputNode ->
            InputNode.state inputNode |> S.map (InputNode title)

        OutputNode title outputNode ->
            OutputNode.state outputNode |> S.map (OutputNode title)

        ExeNode exeNode ->
            ExeNode.state exeNode |> S.map ExeNode


resolveAllReadBlocked : Acc -> Acc
resolveAllReadBlocked acc =
    Dict.foldl resolveReadBlocked { acc | readBlocked = Dict.empty } acc.readBlocked


resolveReadBlocked :
    Addr
    -> ReadBlockedNode
    -> WriteBlockedAcc a
    -> WriteBlockedAcc a
resolveReadBlocked addr ( node, dir, cont ) acc =
    case readAndUnblock addr dir acc of
        Just ( num, acc2 ) ->
            addToCompleted addr (cont num) acc2

        Nothing ->
            addToCompleted addr node acc


readAndUnblock :
    Addr
    -> Dir4
    -> WriteBlockedAcc a
    -> Maybe ( Num, WriteBlockedAcc a )
readAndUnblock rAddr rDir acc =
    moveInDir4 rDir rAddr
        |> getEntryIn acc.writeBlocked
        |> maybeFilter
            (\( _, wbNode ) ->
                rDir == oppositeDir4 wbNode.dir
            )
        |> Maybe.map
            (\( wAddr, wbNode ) ->
                ( wbNode.num
                , completeWriteBlocked wAddr (wbNode.cont ()) acc
                )
            )


resolveAllWriteBlocked : WriteBlockedAcc a -> Store
resolveAllWriteBlocked acc =
    Dict.foldl (\addr { node } -> Dict.insert addr node) acc.completed acc.writeBlocked


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
    -> Node
    -> Num
    -> Dir4
    -> (() -> Node)
    -> { a | writeBlocked : WriteBlockedStore }
    -> { a | writeBlocked : WriteBlockedStore }
addToWriteBlocked addr node num dir cont acc =
    let
        wbn =
            WriteBlockedNode node num dir cont
    in
    { acc | writeBlocked = Dict.insert addr wbn acc.writeBlocked }


addToCompleted :
    Addr
    -> Node
    -> { a | completed : Store }
    -> { a | completed : Store }
addToCompleted na n acc =
    { acc | completed = Dict.insert na n acc.completed }


completeWriteBlocked : Addr -> Node -> WriteBlockedAcc a -> WriteBlockedAcc a
completeWriteBlocked addr node acc =
    { acc | writeBlocked = Dict.remove addr acc.writeBlocked }
        |> addToCompleted addr node


addToReadBlocked :
    Addr
    -> Node
    -> Dir4
    -> (Num -> Node)
    -> { a | readBlocked : ReadBlockedStore }
    -> { a | readBlocked : ReadBlockedStore }
addToReadBlocked addr node dir cont acc =
    { acc | readBlocked = Dict.insert addr ( node, dir, cont ) acc.readBlocked }


view : Sim -> Html msg
view sim =
    fCol [ h100, pa "10px", fontSize "10px", bold, ffMonospace ]
        [ div [] [ text "Cycle: ", text (fromInt sim.cycle) ]
        , viewGrid sim
        ]


viewGrid : Sim -> Html msg
viewGrid sim =
    let
        gapSize =
            "5ch"

        nodeSize =
            "24ch"

        repeatRows =
            fromInt (maxY - 1)

        repeatCols =
            fromInt (maxX - 1)
    in
    div
        [ displayGrid
        , [ "repeat(", repeatRows, ",", gapSize, nodeSize, ")", gapSize ]
            |> String.join " "
            |> gridTemplateRows
        , [ "repeat(", repeatCols, ",", nodeSize, gapSize, ")", nodeSize ]
            |> String.join " "
            |> gridTemplateColumns
        , sMaxHeight "100vh"
        ]
        (viewNodes sim ++ viewPorts sim)


viewPorts : Sim -> List (Html msg)
viewPorts sim =
    toPorts sim |> List.map viewPort


viewNodes : Sim -> List (Html msg)
viewNodes sim =
    Dict.toList sim.store |> List.map viewNode


maxX =
    4


maxY =
    4


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
            gtCols 2
                [ nodeAddrToGridArea addr
                , placeItemsCenter
                ]
                [ div [ tac, fg (grayN 0.7) ]
                    [ div [] [ text title ]
                    , div [] [ text "(IDLE 0%)" ]
                    ]
                ]

        OutputNode title _ ->
            gtCols 2
                [ nodeAddrToGridArea addr
                , placeItemsCenter
                ]
                [ div [ tac, fg (grayN 0.7) ] [ text title ]
                ]

        ExeNode _ ->
            div
                [ nodeAddrToGridArea addr
                , sOutline ("1px solid " ++ grayN 0.7)
                , pa "1ch"
                , tac
                ]
                [ div [ fg <| grayN 0.7 ] [ text "MODE" ]
                , div [] [ text (nodeModeAsString node) ]
                ]


nodeModeAsString : Node -> String
nodeModeAsString node =
    case nodeState node of
        S.ReadyToRun _ ->
            "RUN"

        S.ReadBlocked _ _ ->
            "READ"

        S.WriteBlocked _ _ _ ->
            "WRITE"

        S.Done ->
            "IDLE"


type ArrowType
    = Filled
    | Outline


defaultArrowType =
    Filled


arrowDefault =
    case defaultArrowType of
        Filled ->
            arrowFilled

        Outline ->
            arrowOutline


arrowOutline : Dir4 -> String
arrowOutline dir4 =
    case dir4 of
        Up ->
            "⇧"

        Down ->
            "⇩"

        Left ->
            "⇦ "

        Right ->
            "⇨"


arrowFilled : Dir4 -> String
arrowFilled dir4 =
    case dir4 of
        Up ->
            "⬆"

        Down ->
            "⬇"

        Left ->
            "⬅"

        Right ->
            "➡"
