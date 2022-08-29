module TIS100.Sim exposing
    ( Model
    , Msg
    , sampleModel
    , step
    , update
    , view
    )

import Dict exposing (Dict)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as S exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import TIS100.SelectionList as SelectionList exposing (SelectionList)
import Utils exposing (..)



-- MAIN


sampleModel : Model
sampleModel =
    let
        es =
            [ ( ( 0, 1 ), ExeNode.initMovUpDown )
            , ( ( 0, 2 ), ExeNode.initMovUpDown )
            , ( ( 0, 3 ), ExeNode.initMovUpDown )
            , ( ( 1, 1 ), ExeNode.initMovUpDown )

            --   , ( ( 1, 2 ), ExeNode.initMovUpDown )
            , ( ( 1, 3 ), ExeNode.initMovUpDown )
            , ( ( 2, 1 ), ExeNode.initMov Down Up )

            --   , ( ( 2, 2 ), ExeNode.initMovUpDown )
            , ( ( 2, 3 ), ExeNode.initMov Down Up )

            --   , ( ( 3, 1 ), ExeNode.initMovUpDown )
            --   , ( ( 3, 2 ), ExeNode.initMovUpDown )
            --   , ( ( 3, 3 ), ExeNode.initMovUpDown )
            ]
    in
    init Puzzle.samplePuzzle es



-- MODEL


type alias Model =
    { puzzle : Puzzle
    , initialExecutableNodes : List ( Addr, ExeNode )
    , state : State
    }


type State
    = Debug Sim
    | Edit


init : Puzzle -> List ( Addr, ExeNode ) -> Model
init puzzle es =
    let
        exeGrid =
            exeAddresses
                |> List.map (pairTo ExeNode.empty)
                |> Dict.fromList

        mergedES =
            List.foldl insertEntry exeGrid es |> Dict.toList
    in
    Model puzzle mergedES Edit


type Msg
    = STOP
    | STEP
    | RUN
    | FAST


subscriptions : Sim -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> Model
update msg model =
    case msg of
        STOP ->
            case model.state of
                Debug _ ->
                    { model | state = Edit }

                Edit ->
                    model

        STEP ->
            case model.state of
                Debug sim ->
                    { model | state = Debug (step sim) }

                Edit ->
                    { model
                        | state =
                            Debug
                                (initSim model.puzzle model.initialExecutableNodes)
                    }

        RUN ->
            model

        FAST ->
            model


getCycle : Model -> Maybe Int
getCycle model =
    case model.state of
        Debug sim ->
            Just sim.cycle

        Edit ->
            Nothing


type alias InputColumnViewModel =
    { title : String
    , nums : SelectionList Num
    }


inputDataList : Model -> List InputColumnViewModel
inputDataList { puzzle, state } =
    case state of
        Debug sim ->
            inputColumnViewModels sim

        Edit ->
            puzzle.inputs
                |> List.map
                    (\{ title, nums } ->
                        InputColumnViewModel title (SelectionList.None nums)
                    )


type alias OutputData =
    { title : String
    , expected : SelectionList Num
    , actual : List Num
    }


outputDataList : Model -> List OutputData
outputDataList { puzzle, state } =
    case state of
        Debug sim ->
            outputDataListFromSim sim

        Edit ->
            puzzle.outputs
                |> List.map
                    (\{ title, nums } ->
                        OutputData title (SelectionList.None nums) []
                    )


view : Model -> Html Msg
view model =
    fCol
        [ h100
        , fontSize "12px"
        , styleLineHeight "0.9"
        , pa "2ch"
        , bold
        , ffMonospace
        , gap "2ch"
        , ttu
        ]
        [ viewCycle (getCycle model)
        , fRow [ gap "2ch" ]
            [ fCol [ sWidth "40ch", gap "2ch", fg lightGray ]
                [ div [] [ viewTitle, viewDesc ]
                , viewIOColumns model
                , viewButtons
                ]
            , viewGrid (viewGridItems model)
            ]
        ]


viewGridItems : Model -> List (Html msg)
viewGridItems { puzzle, initialExecutableNodes, state } =
    case state of
        Debug sim ->
            viewSimGridItems sim

        Edit ->
            viewEditNodes puzzle initialExecutableNodes
                ++ viewEditPorts puzzle initialExecutableNodes


viewEditNodes : Puzzle -> List ( Addr, ExeNode ) -> List (Html msg)
viewEditNodes puzzle es =
    List.map viewInputNode puzzle.inputs
        ++ List.map viewOutputNode puzzle.outputs
        ++ List.map viewExeNodeEntry es


viewEditPorts : Puzzle -> List ( Addr, ExeNode ) -> List (Html msg)
viewEditPorts puzzle es =
    List.map (\{ x } -> ( ( x, 0 ), [ Write Down ] )) puzzle.inputs
        ++ List.map (\{ x } -> ( ( x, maxY ), [ Read Up ] )) puzzle.outputs
        ++ List.map (mapSecond ExeNode.ioIntents) es
        |> List.concatMap (\( addr, ioIntents ) -> initEmptyPorts addr ioIntents)
        |> mergePorts
        |> List.map viewPort


viewCycle : Maybe Int -> Html msg
viewCycle mbCycle =
    let
        cycle =
            case mbCycle of
                Nothing ->
                    "NA"

                Just c ->
                    fromInt c
    in
    div [] [ text "Cycle: ", text cycle ]



-- ADDR


type alias Addr =
    ( Int, Int )


maxX : Int
maxX =
    4


maxY : Int
maxY =
    4



-- NODE


type Node
    = InputNode IOConfig InputNode
    | OutputNode IOConfig OutputNode
    | ExeNode ExeNode


initInputNode : IOConfig -> Node
initInputNode conf =
    InputNode conf (InputNode.fromList conf.nums)


initOutputNode : IOConfig -> Node
initOutputNode conf =
    OutputNode conf (OutputNode.fromExpected (List.length conf.nums))


nodeIoIntents : Node -> List IOIntent
nodeIoIntents node =
    case node of
        InputNode _ _ ->
            [ Write Down ]

        OutputNode _ _ ->
            [ Read Up ]

        ExeNode exe ->
            ExeNode.ioIntents exe


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        InputNode title inputNode ->
            InputNode.state inputNode |> S.map (InputNode title)

        OutputNode conf outputNode ->
            OutputNode.state outputNode |> S.map (OutputNode conf)

        ExeNode exeNode ->
            ExeNode.state exeNode |> S.map ExeNode


viewNodeEntry : NodeEntry -> Html msg
viewNodeEntry ( addr, node ) =
    case node of
        InputNode conf _ ->
            viewInputNode conf

        OutputNode conf _ ->
            viewOutputNode conf

        ExeNode exe ->
            viewExeNodeEntry ( addr, exe )


viewInputNode : IOConfig -> Html msg
viewInputNode { x, title } =
    gtCols 2
        [ gridAreaXY ( x * 2, 0 )
        , placeItemsCenter
        ]
        [ div [ tac, fg lightGray ]
            [ div [] [ text title ]
            , div [] [ text "(IDLE 0%)" ]
            ]
        ]


viewOutputNode : IOConfig -> Html msg
viewOutputNode { x, title } =
    gtCols 2
        [ gridAreaXY ( x * 2, maxY * 2 - 2 )
        , placeItemsCenter
        ]
        [ div [ tac, fg lightGray ] [ text title ]
        ]


viewExeNodeEntry : ( Addr, ExeNode ) -> Html msg
viewExeNodeEntry ( ( x, y ), exe ) =
    div
        [ gridAreaXY ( x * 2, y * 2 - 1 )
        , lightOutline
        , dGrid
        , gridAutoFlowColumn
        ]
        [ div [ sWidth "18ch", pa "1ch" ] [ text (ExeNode.toSource exe) ]
        , gtRows 5
            []
            [ viewExeBox "ACC" "0"
            , viewExeBox "BAK" "<0>"
            , viewExeBox "LAST" "N/A"
            , viewExeBox "MODE" (exeMode exe)
            , viewExeBox "IDLE" "0%"
            ]
        ]


viewExeBox : String -> String -> Html msg
viewExeBox a b =
    div [ dGrid, tac, placeContentCenter, sOutline ("1px solid " ++ lightGray) ]
        [ div [ fg lightGray ] [ text a ]
        , div [] [ text b ]
        ]



--noinspection SpellCheckingInspection


writeModeLabel =
    -- spelling needs to be 4ch for alignment
    "WRTE"


exeMode : ExeNode -> String
exeMode exe =
    case ExeNode.state exe of
        S.ReadyToRun _ ->
            "RUN"

        S.ReadBlocked _ _ ->
            "READ"

        S.WriteBlocked _ _ _ ->
            writeModeLabel

        S.Done ->
            "IDLE"


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



-- SIM


type alias NodeEntry =
    ( Addr, Node )


type alias Store =
    Dict Addr Node


type alias Sim =
    { store : Store
    , cycle : Int
    }


initSim : Puzzle -> List ( Addr, ExeNode ) -> Sim
initSim puzzle es =
    let
        store =
            initialStore
                |> withInputs puzzle.inputs
                |> withOutputs puzzle.outputs
                |> withExecutables es
    in
    { store = store
    , cycle = 0
    }


initialStore : Store
initialStore =
    exeAddresses
        |> List.map (pairTo (ExeNode ExeNode.empty))
        |> Dict.fromList


exeAddresses : List Addr
exeAddresses =
    [ ( 0, 1 )
    , ( 0, 2 )
    , ( 0, 3 )
    , ( 1, 1 )
    , ( 1, 2 )
    , ( 1, 3 )
    , ( 2, 1 )
    , ( 2, 2 )
    , ( 2, 3 )
    , ( 3, 1 )
    , ( 3, 2 )
    , ( 3, 3 )
    ]


withInputs : List IOConfig -> Store -> Store
withInputs list store =
    List.foldl
        (\conf -> Dict.insert ( conf.x, 0 ) (initInputNode conf))
        store
        list


withOutputs : List IOConfig -> Store -> Store
withOutputs list store =
    List.foldl
        (\conf ->
            Dict.insert ( conf.x, maxY ) (initOutputNode conf)
        )
        store
        list


withExecutables : List ( Addr, ExeNode ) -> Store -> Store
withExecutables list store =
    List.foldl
        (\( addr, exe ) -> Dict.insert addr (ExeNode exe))
        store
        list


inputColumnViewModels : Sim -> List InputColumnViewModel
inputColumnViewModels sim =
    let
        mapper node =
            case node of
                InputNode conf inputNode ->
                    Just
                        (InputColumnViewModel conf.title
                            (InputNode.toSelectionList inputNode)
                        )

                _ ->
                    Nothing
    in
    Dict.values sim.store |> List.filterMap mapper


outputDataListFromSim : Sim -> List OutputData
outputDataListFromSim sim =
    let
        mapper node =
            case node of
                OutputNode conf outputNode ->
                    let
                        actual =
                            OutputNode.getNumsRead outputNode
                    in
                    Just <|
                        OutputData
                            conf.title
                            (SelectionList.fromIndex (List.length actual) conf.nums)
                            actual

                _ ->
                    Nothing
    in
    Dict.values sim.store |> List.filterMap mapper


simIOIntentsAndNodeState : Sim -> Dict Addr ( List IOIntent, NodeState Node )
simIOIntentsAndNodeState sim =
    Dict.map (\_ node -> ( nodeIoIntents node, nodeState node )) sim.store



-- SIM UPDATE


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


type alias ReadBlockedStore =
    Dict Addr ReadBlockedNode


type alias ReadBlockedNode =
    ( Node, Dir4, Num -> Node )


type alias WriteBlockedStore =
    Dict Addr WriteBlockedNode


type alias WriteBlockedNode =
    { node : Node, num : Num, dir : Dir4, cont : () -> Node }


emptyAcc : Acc
emptyAcc =
    { readBlocked = Dict.empty
    , writeBlocked = Dict.empty
    , completed = Dict.empty
    }


addToReadBlocked :
    Addr
    -> Node
    -> Dir4
    -> (Num -> Node)
    -> { a | readBlocked : ReadBlockedStore }
    -> { a | readBlocked : ReadBlockedStore }
addToReadBlocked addr node dir cont acc =
    { acc | readBlocked = Dict.insert addr ( node, dir, cont ) acc.readBlocked }


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



-- SIM VIEW
--viewSim : Sim -> Html Msg
--viewSim sim =
--    fCol
--        [ h100
--        , fontSize "12px"
--        , styleLineHeight "0.9"
--        , pa "2ch"
--        , bold
--        , ffMonospace
--        , gap "2ch"
--        , ttu
--        ]
--        [ div [] [ text "Cycle: ", text (fromInt sim.cycle) ]
--        , fRow [ gap "2ch" ]
--            [ fCol [ sWidth "40ch", gap "2ch", fg lightGray ]
--                [ div [] [ viewTitle, viewDesc ]
--                , viewIOColumns sim
--                , viewButtons
--                ]
--            , viewGrid (viewSimGridItems sim)
--            ]
--        ]


viewButtons : Html Msg
viewButtons =
    gtCols 4
        [ gap "2ch" ]
        [ btn "stop" STOP
        , btn "step" STEP
        , btn "run" RUN
        , btn "fast" FAST
        ]


btn : String -> msg -> Html msg
btn txt msg =
    button
        [ lightOutline
        , bgc "inherit"
        , fg "inherit"
        , style "text-transform" "inherit"
        , style "font" "inherit"
        , borderNone
        , dGrid
        , placeContentCenter
        , aspectRatio "1"
        , notifyClick msg
        ]
        [ text txt ]


viewTitle =
    div [ tac, styleLineHeight "2" ] [ text "-- Title --" ]


viewDesc =
    fCol
        [ lightOutline
        , pa "0.5ch"
        , placeContentCenter
        ]
        (List.repeat 6 (div [] [ text "> desc" ]))


viewIOColumns : Model -> Html msg
viewIOColumns model =
    fRow [ tac, gap "2ch" ]
        (List.map viewInputColumn (inputDataList model)
            ++ List.map viewOutputColumn (outputDataList model)
        )


viewInputColumn : InputColumnViewModel -> Html msg
viewInputColumn { title, nums } =
    let
        numViews =
            Num.viewSelectionList nums
    in
    fCol [ gap "0.5ch" ]
        [ div [] [ text title ]
        , div
            [ lightOutline
            , sWidth "4ch"
            , pa "0.5ch 0"
            , styleLineHeight "0.8"
            ]
            (times 39
                (\i ->
                    listGetAt i numViews
                        |> Maybe.withDefault (div [] [ text nbsp ])
                )
            )
        ]


viewOutputColumn : OutputData -> Html msg
viewOutputColumn { title, expected, actual } =
    let
        expectedViews =
            Num.viewSelectionList expected

        actualViews =
            List.map Num.view actual
    in
    fCol [ gap "0.5ch" ]
        [ div [] [ text title ]
        , fRow []
            [ div
                [ lightOutline
                , sWidth "4ch"
                , pa "0.5ch 0"
                , styleLineHeight "0.8"
                ]
                (times 39
                    (\i ->
                        listGetAt i expectedViews
                            |> Maybe.withDefault (div [] [ text nbsp ])
                    )
                )
            , div
                [ lightOutline
                , sWidth "4ch"
                , pa "0.5ch 0"
                , styleLineHeight "0.8"
                ]
                (times 39
                    (\i ->
                        listGetAt i actualViews
                            |> Maybe.withDefault (div [] [ text nbsp ])
                    )
                )
            ]
        ]


nbsp : String
nbsp =
    "\u{00A0}"


viewGrid : List (Html msg) -> Html msg
viewGrid =
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


viewSimGridItems : Sim -> List (Html msg)
viewSimGridItems sim =
    List.map viewNodeEntry (Dict.toList sim.store)
        ++ viewPorts (simIOIntentsAndNodeState sim)



-- PORT ID


type alias PortKey =
    ( Addr, Addr )


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



-- PORT VALUE


type PortValue
    = Empty
    | Num Num
    | Queried


mergePortValue : PortValue -> PortValue -> PortValue
mergePortValue old new =
    case ( old, new ) of
        ( Empty, _ ) ->
            new

        ( _, Empty ) ->
            old

        ( _, Queried ) ->
            old

        _ ->
            new


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



-- PORT


type Port
    = Port PortId PortValue


initEmptyPorts : Addr -> List IOIntent -> List Port
initEmptyPorts addr iOIntents =
    List.filterMap (initPortId addr) iOIntents
        |> List.map (\id -> Port id Empty)


initPort : Addr -> ( IOIntent, PortValue ) -> Maybe Port
initPort addr ( iOIntent, portValue ) =
    initPortId addr iOIntent
        |> Maybe.map (\id -> Port id portValue)


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
            [ viewArrow Down portValue
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
            , viewArrow Up portValue
            ]
        ]


viewArrow : Dir4 -> PortValue -> Html msg
viewArrow dir4 pv =
    let
        color =
            case pv of
                Empty ->
                    darkGray

                _ ->
                    "inherit"
    in
    span [ fg color, fontSize "2em", fontWeight "100" ] [ text (arrowDefault dir4) ]



-- PORTS


viewPorts : Dict Addr ( List IOIntent, NodeState a ) -> List (Html msg)
viewPorts dict =
    initPorts dict |> List.map viewPort


type alias Ports =
    Dict PortKey Port


initPorts : Dict Addr ( List IOIntent, NodeState a ) -> List Port
initPorts dict =
    Dict.toList dict
        |> List.concatMap initPortsHelp
        |> mergePorts


initPortsHelp : ( Addr, ( List IOIntent, NodeState a ) ) -> List Port
initPortsHelp ( addr, ( ioIntents, nState ) ) =
    let
        ioFromNodeState : List ( IOIntent, PortValue )
        ioFromNodeState =
            if second addr == maxY then
                -- ignore updating read query for output node
                []

            else
                case nState of
                    S.ReadyToRun _ ->
                        []

                    S.ReadBlocked dir _ ->
                        [ ( Read dir, Queried ) ]

                    S.WriteBlocked num dir _ ->
                        [ ( Write dir, Num num ) ]

                    S.Done ->
                        []

        ioFromIntents =
            ioIntents |> List.map (pairTo Empty)
    in
    ioFromNodeState
        ++ ioFromIntents
        |> List.filterMap (initPort addr)


mergePorts : List Port -> List Port
mergePorts =
    let
        merge : Port -> Ports -> Ports
        merge ((Port id newVal) as port_) =
            Dict.update (portKeyFromId id)
                (\mbOld ->
                    case mbOld of
                        Nothing ->
                            Just port_

                        Just (Port _ oldVal) ->
                            Just (Port id (mergePortValue oldVal newVal))
                )
    in
    List.foldl merge Dict.empty >> Dict.values



-- UI HELPERS


darkGray =
    grayN 0.5


lightGray =
    grayN 0.7


lightOutline =
    sOutline ("1px solid " ++ lightGray)


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
