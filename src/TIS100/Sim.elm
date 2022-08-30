module TIS100.Sim exposing
    ( Model
    , Msg
    , sampleModel
    , update
    , view
    )

import Dict exposing (Dict)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.IOIntent exposing (IOAction(..), IOIntent(..))
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as S exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import TIS100.Ports as Ports
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import TIS100.SelectionList as SelectionList exposing (SelectionList)
import TIS100.UI as UI
import Utils exposing (..)



-- MAIN


sampleModel : Model
sampleModel =
    let
        es =
            [ --( ( 0, 1 ), ExeNode.initMovUpDown )
              ( ( 0, 1 ), ExeNode.initMov Right Down )
            , ( ( 0, 2 ), ExeNode.initMovUpDown )
            , ( ( 0, 3 ), ExeNode.initMovUpDown )
            , ( ( 1, 1 ), ExeNode.initMovUpDown )
            , ( ( 1, 2 ), ExeNode.empty )
            , ( ( 1, 3 ), ExeNode.initMovUpDown )
            , ( ( 2, 1 ), ExeNode.initMov Down Up )
            , ( ( 2, 2 ), ExeNode.empty )
            , ( ( 2, 3 ), ExeNode.initMov Down Up )
            , ( ( 3, 1 ), ExeNode.initMov Left Down )
            , ( ( 3, 2 ), ExeNode.initMov Up Right )
            , ( ( 3, 3 ), ExeNode.initMov Left Down )
            ]
    in
    init Puzzle.samplePuzzle es



-- MODEL


type alias Model =
    { puzzle : Puzzle
    , editDict : EditDict
    , state : State
    }


type alias EditDict =
    Dict Addr ExeNode


type State
    = Debug Sim
    | Edit


init : Puzzle -> List ( Addr, ExeNode ) -> Model
init puzzle es =
    let
        editDict =
            editAddressesFromLayout puzzle.layout
                |> List.foldl Dict.remove initialEditDict

        mergedEditDict : EditDict
        mergedEditDict =
            List.foldl replaceEntry editDict es
    in
    Model puzzle mergedEditDict Edit


initialEditDict : EditDict
initialEditDict =
    List.foldl (\k -> Dict.insert k ExeNode.empty) Dict.empty editAddresses


editAddressesFromLayout : List Puzzle.NodeType -> List Addr
editAddressesFromLayout ls =
    List.indexedMap
        (\i nt ->
            case nt of
                Puzzle.Executable ->
                    Nothing

                Puzzle.Faulty ->
                    Just ( modBy 4 i, i // 4 + 1 )
        )
        ls
        |> List.filterMap identity


editAddresses : List Addr
editAddresses =
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
                    { model | state = Debug (initSim model.puzzle model.editDict) }

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


inputColumnViewModel : Model -> List InputColumnViewModel
inputColumnViewModel { puzzle, state } =
    case state of
        Debug sim ->
            inputColumnViewModels sim

        Edit ->
            puzzle.inputs
                |> List.map
                    (\{ title, nums } ->
                        InputColumnViewModel title (SelectionList.None nums)
                    )


type alias OutputColumnViewModel =
    { title : String
    , expected : SelectionList Num
    , actual : List Num
    }


outputColumnViewModel : Model -> List OutputColumnViewModel
outputColumnViewModel { puzzle, state } =
    case state of
        Debug sim ->
            outputDataListFromSim sim

        Edit ->
            puzzle.outputs
                |> List.map
                    (\{ title, nums } ->
                        OutputColumnViewModel title (SelectionList.None nums) []
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
viewGridItems { puzzle, editDict, state } =
    case state of
        Debug sim ->
            List.map viewNodeEntry (Dict.toList sim.store)
                ++ Ports.view puzzle (simIntentsAndActions sim)

        Edit ->
            let
                es =
                    Dict.toList editDict
            in
            viewEditNodes puzzle es
                ++ Ports.viewAllPorts puzzle


viewEditNodes : Puzzle -> List ( Addr, ExeNode ) -> List (Html msg)
viewEditNodes puzzle es =
    List.map viewInputNode puzzle.inputs
        ++ List.map viewOutputNode puzzle.outputs
        ++ List.map viewExeNodeEntry es


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
        (List.map viewInputColumn (inputColumnViewModel model)
            ++ List.map viewOutputColumn (outputColumnViewModel model)
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


viewOutputColumn : OutputColumnViewModel -> Html msg
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


inputNodeEntry : IOConfig -> ( Addr, Node )
inputNodeEntry conf =
    ( ( conf.x, 0 ), initInputNode conf )


initOutputNode : IOConfig -> Node
initOutputNode conf =
    OutputNode conf (OutputNode.fromExpected (List.length conf.nums))


outputNodeEntry : IOConfig -> ( Addr, Node )
outputNodeEntry conf =
    ( ( conf.x, maxY ), initOutputNode conf )


nodeIoIntents : Node -> List IOIntent
nodeIoIntents node =
    case node of
        InputNode _ _ ->
            [ Write Down ]

        OutputNode _ _ ->
            [ Read Up ]

        ExeNode exe ->
            ExeNode.ioIntents exe


nodeIoActions : Node -> List IOAction
nodeIoActions node =
    case nodeState node of
        S.ReadyToRun _ ->
            []

        S.ReadBlocked dir4 _ ->
            [ Reading dir4 ]

        S.WriteBlocked num dir4 _ ->
            [ Writing dir4 num ]

        S.Done ->
            []


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



-- SIM


type alias NodeEntry =
    ( Addr, Node )


type alias Store =
    Dict Addr Node


type alias Sim =
    { store : Store
    , cycle : Int
    }


initSim : Puzzle -> EditDict -> Sim
initSim puzzle editDict =
    let
        store =
            List.map inputNodeEntry puzzle.inputs
                ++ List.map outputNodeEntry puzzle.outputs
                |> Dict.fromList
                |> Dict.union (Dict.map (always ExeNode) editDict)
    in
    { store = store
    , cycle = 0
    }


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


outputDataListFromSim : Sim -> List OutputColumnViewModel
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
                        OutputColumnViewModel
                            conf.title
                            (SelectionList.fromIndex (List.length actual) conf.nums)
                            actual

                _ ->
                    Nothing
    in
    Dict.values sim.store |> List.filterMap mapper


simIntentsAndActions : Sim -> ( List ( Addr, IOIntent ), List ( Addr, IOAction ) )
simIntentsAndActions sim =
    Dict.foldl
        (\addr node ( intents, actions ) ->
            ( List.map (pair addr) (nodeIoIntents node) ++ intents
            , List.map (pair addr) (nodeIoActions node) ++ actions
            )
        )
        ( [], [] )
        sim.store



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



-- UI HELPERS


lightGray =
    UI.lightGray


lightOutline =
    UI.lightOutline
