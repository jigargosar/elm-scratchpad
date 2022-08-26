module TIS100.Sim exposing
    ( Sim
    , init
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


parseAddr : Addr -> Maybe Addr
parseAddr addr =
    if mapBoth (clamp 0 maxX) (clamp 0 maxY) addr == addr then
        Just addr

    else
        Nothing


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
    = Port PortId PortValue


type PortValue
    = Empty
    | Num Num
    | Queried


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


portIdFromPotentialIO : Addr -> IOIntent -> Maybe PortId
portIdFromPotentialIO addr potentialIO =
    case potentialIO of
        Read dir ->
            moveAddrBy dir addr
                |> Maybe.map
                    (\oppAddr -> PortId oppAddr (oppositeDir4 dir) ( oppAddr, addr ))

        Write dir ->
            moveAddrBy dir addr |> Maybe.map (pair addr >> PortId addr dir)


portIdForWriting : Addr -> Dir4 -> Maybe PortId
portIdForWriting addr dir =
    moveAddrBy dir addr |> Maybe.map (pair addr >> PortId addr dir)


portIdForReading : Addr -> Dir4 -> Maybe PortId
portIdForReading addr dir =
    moveAddrBy dir addr
        |> Maybe.map
            (\oppAddr -> PortId oppAddr (oppositeDir4 dir) ( oppAddr, addr ))


type alias PortKey =
    ( Addr, Addr )


type Ports
    = Ports (Dict PortKey Port)


emptyPorts : Ports
emptyPorts =
    Ports Dict.empty


getPortList : Sim -> List Port
getPortList sim =
    foldlEntries addPotentialPorts emptyPorts sim.store
        |> updatePortValues sim
        |> portsToList


updatePortValues : Sim -> Ports -> Ports
updatePortValues sim ports =
    foldlEntries updatePortValuesFromNode ports sim.store


updatePortValuesFromNode : NodeEntry -> Ports -> Ports
updatePortValuesFromNode ( addr, node ) =
    case nodeState node of
        S.ReadyToRun _ ->
            identity

        S.ReadBlocked dir _ ->
            if isOutputNode node then
                identity

            else
                queryPort addr dir

        S.WriteBlocked num dir _ ->
            writeToPort addr dir num

        S.Done ->
            identity


isOutputNode : Node -> Bool
isOutputNode node =
    case node of
        OutputNode _ _ ->
            True

        _ ->
            False


addPotentialPorts : NodeEntry -> Ports -> Ports
addPotentialPorts ( addr, node ) =
    case node of
        InputNode _ _ ->
            addIOIntent addr (Write Down)

        OutputNode _ _ ->
            addIOIntent addr (Read Up)

        ExeNode exe ->
            addIOIntents addr (ExeNode.ioIntents exe)


addIOIntents : Addr -> List IOIntent -> Ports -> Ports
addIOIntents addr ioIntents ports =
    List.foldl (addIOIntent addr) ports ioIntents


addIOIntent : Addr -> IOIntent -> Ports -> Ports
addIOIntent addr potentialIO ports =
    case portIdFromPotentialIO addr potentialIO of
        Nothing ->
            ports

        Just pid ->
            addPotentialPort pid ports


addPotentialPort : PortId -> Ports -> Ports
addPotentialPort ((PortId _ _ portKey) as portId) ((Ports dict) as ports) =
    if Dict.member portKey dict then
        ports

    else
        Ports (Dict.insert portKey (Port portId Empty) dict)


writeToPort : Addr -> Dir4 -> Num -> Ports -> Ports
writeToPort addr dir num ((Ports dict) as ports) =
    case portIdForWriting addr dir of
        Nothing ->
            ports

        Just ((PortId _ _ key) as portId) ->
            case Dict.get key dict of
                Nothing ->
                    ports

                Just _ ->
                    Ports (Dict.insert key (Port portId (Num num)) dict)


queryPort : Addr -> Dir4 -> Ports -> Ports
queryPort addr dir ((Ports dict) as ports) =
    case portIdForReading addr dir of
        Nothing ->
            ports

        Just ((PortId _ _ key) as portId) ->
            case Dict.get key dict of
                Just (Port _ Empty) ->
                    Ports (Dict.insert key (Port portId Queried) dict)

                _ ->
                    ports


moveAddrBy : Dir4 -> Addr -> Maybe Addr
moveAddrBy dir addr =
    moveInDir4 dir addr |> parseAddr


portsToList : Ports -> List Port
portsToList (Ports dict) =
    Dict.values dict


viewPort : Port -> Html msg
viewPort (Port (PortId (( _, y ) as addr) dir _) portValue) =
    case dir of
        Up ->
            if y <= 1 || y >= maxY then
                noView

            else
                viewUpPortValue addr portValue

        Down ->
            if y < 0 || y >= maxY then
                noView

            else
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
        S.WriteBlocked num dir cont ->
            addToWriteBlocked addr (WriteBlockedNode node num dir cont)

        S.Done ->
            addToCompleted addr node

        S.ReadBlocked dir readResolver ->
            addToReadBlocked addr ( node, dir, readResolver )

        S.ReadyToRun runResolver ->
            resolveAfterRun addr (runResolver ())


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        InputNode title inputNode ->
            InputNode.state inputNode |> S.map (InputNode title)

        OutputNode title outputNode ->
            OutputNode.state outputNode |> S.map (OutputNode title)

        ExeNode exeNode ->
            ExeNode.state exeNode |> S.map ExeNode


resolveAfterRun : Addr -> Node -> Acc -> Acc
resolveAfterRun addr node =
    case nodeState node of
        S.ReadBlocked dir cont ->
            addToReadBlocked addr ( node, dir, cont )

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
readAndUnblock readerAddr readDir acc =
    moveAddrBy readDir readerAddr
        |> Maybe.andThen (getEntryIn acc.writeBlocked)
        |> maybeFilter
            (\( _, writeBlockedNode ) ->
                readDir == oppositeDir4 writeBlockedNode.dir
            )
        |> Maybe.map
            (\( writerAddr, { num, cont } ) ->
                ( num
                , addToCompleted
                    writerAddr
                    (cont ())
                    { acc
                        | writeBlocked = Dict.remove writerAddr acc.writeBlocked
                    }
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
    getPortList sim |> List.map viewPort


viewNodes : Sim -> List (Html msg)
viewNodes sim =
    Dict.toList sim.store |> List.map viewNode


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


nodeBlockMode : Node -> String
nodeBlockMode node =
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
