module TIS100.PuzzlePage exposing
    ( Model
    , Msg
    , sampleModel
    , subscriptions
    , update
    , view
    )

import Dict exposing (Dict)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.Grid as Grid exposing (Addr, Cell(..))
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as S exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import TIS100.Ports as Ports exposing (Action(..), Intent(..))
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import TIS100.SelectionList as SelectionList exposing (SelectionList)
import TIS100.UI as UI
import Time
import Utils exposing (..)



-- MAIN


sampleModel : Model
sampleModel =
    let
        es =
            [ ( ( 0, 1 ), ExeNode.initMov Up Down )
            , ( ( 0, 2 ), ExeNode.initMovUpDown )
            , ( ( 0, 3 ), ExeNode.initMovUpDown )
            , ( ( 1, 1 ), ExeNode.initMov Right Down )
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
    , editors : Dict Addr Editor
    , exs : List ( Addr, ExeNode )
    , state : State
    }


type alias Editor =
    String


type State
    = Edit
    | Sim_ Sim


init : Puzzle -> List ( Addr, ExeNode ) -> Model
init puzzle exs =
    let
        emptyEditors : Dict Addr String
        emptyEditors =
            Dict.toList puzzle.layout
                |> List.filterMap
                    (\( a, n ) ->
                        case n of
                            Puzzle.Executable ->
                                Just ( a, "" )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList

        editors =
            replaceEntries (List.map (mapSecond ExeNode.toSource) exs) emptyEditors
    in
    { puzzle = puzzle
    , editors = editors
    , exs = exs
    , state = Edit
    }


type Msg
    = STOP
    | STEP
    | RUN
    | FAST
    | AutoStep


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Edit ->
            Sub.none

        Sim_ sim ->
            case sim.state of
                Stepping debugger ->
                    case debugger of
                        Manual ->
                            Sub.none

                        Auto ->
                            Time.every 50 (\_ -> AutoStep)

                        AutoFast ->
                            Time.every 20 (\_ -> AutoStep)

                Completed ->
                    Sub.none


startDebugging : StepMode -> Model -> Model
startDebugging stepMode model =
    let
        _ =
            "Need to compile edit nodes so we can init sim"
    in
    { model | state = Sim_ (initSim model.puzzle model.exs stepMode) }


startEditing : Model -> Model
startEditing model =
    { model | state = Edit }


update : Msg -> Model -> Model
update msg model =
    case model.state of
        Edit ->
            case msg of
                STOP ->
                    model

                STEP ->
                    startDebugging Manual model

                AutoStep ->
                    model

                RUN ->
                    startDebugging Auto model

                FAST ->
                    startDebugging AutoFast model

        Sim_ sim ->
            case msg of
                STOP ->
                    startEditing model

                STEP ->
                    { model | state = Sim_ (manualStepSim sim) }

                AutoStep ->
                    { model | state = Sim_ (autoStepSim sim) }

                RUN ->
                    { model | state = Sim_ (simSetStepMode Auto sim) }

                FAST ->
                    { model | state = Sim_ (simSetStepMode AutoFast sim) }


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
        [ viewCycle model
        , fRow [ gap "2ch" ]
            [ viewLeftBar model
            , viewGrid model
            ]
        ]


viewCycle : Model -> Html msg
viewCycle model =
    let
        cycleText =
            case model.state of
                Edit ->
                    "NA"

                Sim_ sim ->
                    fromInt sim.cycle
    in
    div [] [ text "Cycle: ", text cycleText ]


viewGrid : Model -> Html msg
viewGrid { puzzle, state, editors } =
    div
        [ displayGrid
        , List.repeat 3 UI.nodeSize |> String.join " " |> gridTemplateRows
        , List.repeat 4 UI.nodeSize |> String.join " " |> gridTemplateColumns
        , paddingXY "0" UI.gapSize
        , gap UI.gapSize
        , sMaxHeight "100vh"
        ]
        (case state of
            Edit ->
                viewEditNodes puzzle editors
                    ++ Ports.viewAllPorts puzzle

            Sim_ { store } ->
                List.map viewSimNode (Dict.toList store)
                    ++ Ports.view puzzle (simIntentsAndActions store)
        )


viewEditNodes : Puzzle -> Dict Addr Editor -> List (Html msg)
viewEditNodes puzzle editors =
    Puzzle.gridToList
        viewInputNode
        viewOutputNode
        (\( addr, nk ) ->
            case nk of
                Puzzle.Executable ->
                    maybeView viewEditor (getEntry addr editors)

                Puzzle.Faulty ->
                    viewFaultyNode addr
        )
        puzzle


toLeftBarViewModel : Model -> LeftBarViewModel
toLeftBarViewModel { puzzle, state } =
    { inputs =
        case state of
            Edit ->
                List.map
                    (\conf ->
                        { title = conf.title
                        , nums = SelectionList.None conf.nums
                        }
                    )
                    puzzle.inputs

            Sim_ { store } ->
                Grid.inputsToList
                    (\_ c i ->
                        { title = c.title
                        , nums = InputNode.toSelectionList i
                        }
                    )
                    store
    , outputs =
        case state of
            Edit ->
                List.map
                    (\conf ->
                        { title = conf.title
                        , expected = SelectionList.None conf.nums
                        , actual = []
                        }
                    )
                    puzzle.outputs

            Sim_ { store } ->
                Grid.outputsToList
                    (\_ c o ->
                        let
                            actual =
                                OutputNode.getNumsRead o
                        in
                        { title = c.title
                        , expected =
                            SelectionList.fromIndex (List.length actual) c.nums
                        , actual = actual
                        }
                    )
                    store
    }


viewLeftBar : Model -> Html Msg
viewLeftBar { puzzle, state } =
    fCol [ sWidth "40ch", gap "2ch", fg lightGray ]
        [ div [] [ viewTitle, viewDesc ]
        , fRow [ tac, gap "2ch" ]
            (case state of
                Edit ->
                    viewIOColumns puzzle

                Sim_ sim ->
                    viewSimIOColumns sim
            )
        , viewButtons
        ]


type alias LeftBarViewModel =
    { inputs : List InputColumnViewModel
    , outputs : List OutputColumnViewModel
    }


type alias InputColumnViewModel =
    { title : String, nums : SelectionList Num }


type alias OutputColumnViewModel =
    { title : String
    , expected : SelectionList Num
    , actual : List Num
    }


viewIOColumns : Puzzle -> List (Html msg)
viewIOColumns puzzle =
    List.map
        (\conf ->
            viewInputColumn
                { title = conf.title
                , nums = SelectionList.None conf.nums
                }
        )
        puzzle.inputs
        ++ List.map
            (\conf ->
                viewOutputColumn
                    { title = conf.title
                    , expected = SelectionList.None conf.nums
                    , actual = []
                    }
            )
            puzzle.outputs


viewSimIOColumns : Sim -> List (Html msg)
viewSimIOColumns { store } =
    Grid.inputsToList
        (\_ c i ->
            viewInputColumn
                { title = c.title
                , nums = InputNode.toSelectionList i
                }
        )
        store
        ++ Grid.outputsToList
            (\_ c o ->
                let
                    actual =
                        OutputNode.getNumsRead o
                in
                viewOutputColumn
                    { title = c.title
                    , expected =
                        SelectionList.fromIndex (List.length actual) c.nums
                    , actual = actual
                    }
            )
            store


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


viewTitle : Html msg
viewTitle =
    div [ tac, styleLineHeight "2" ] [ text "-- Title --" ]


viewDesc : Html msg
viewDesc =
    fCol
        [ lightOutline
        , pa "0.5ch"
        , placeContentCenter
        ]
        (List.repeat 6 (div [] [ text "> desc" ]))


viewInputColumn :
    { title : String
    , nums : SelectionList Num
    }
    -> Html msg
viewInputColumn { title, nums } =
    fCol [ gap "0.5ch" ]
        [ div [] [ text title ]
        , viewNumColumn (Num.viewSelectionList nums)
        ]


viewOutputColumn :
    { title : String
    , expected : SelectionList Num
    , actual : List Num
    }
    -> Html msg
viewOutputColumn { title, expected, actual } =
    fCol [ gap "0.5ch" ]
        [ div [] [ text title ]
        , fRow []
            [ viewNumColumn (Num.viewSelectionList expected)
            , viewNumColumn
                (List.map2
                    (\e a ->
                        if e == a then
                            Num.view a

                        else
                            Num.viewError a
                    )
                    (SelectionList.toList expected)
                    actual
                )
            ]
        ]


viewNumColumn : List (Html msg) -> Html msg
viewNumColumn numViews =
    div
        [ lightOutline
        , sWidth "4ch"
        , pa "0.5ch 0"
        , styleLineHeight "0.8"
        ]
        (numViews
            ++ List.repeat 39 (div [] [ text nbsp ])
            |> List.take 39
        )



-- NODE


viewInputNode : IOConfig -> Html msg
viewInputNode { x, title } =
    div
        [ displayGrid
        , placeContentCenter
        , sWidth "50%"
        , sHeight UI.gapSize
        , gridAreaXY ( x, 0 )
        , positionRelative
        , style "bottom" UI.gapSize
        ]
        [ div [ tac, fg lightGray ]
            [ div [] [ text title ]
            , div [] [ text "(IDLE 0%)" ]
            ]
        ]


viewOutputNode : IOConfig -> Html msg
viewOutputNode { x, title } =
    div
        [ displayGrid
        , placeContentCenter
        , sWidth "50%"
        , sHeight UI.gapSize
        , gridAreaXY ( x, 2 )
        , positionRelative
        , top100
        ]
        [ div [ tac, fg lightGray ]
            [ div [] [ text title ]
            ]
        ]


viewEditor : ( Addr, Editor ) -> Html msg
viewEditor ( ( x, y ), editor ) =
    div
        [ gridAreaXY ( x, y - 1 )
        , lightOutline
        , dGrid
        , gridAutoFlowColumn
        ]
        [ div [ sWidth "18ch", pa "1ch" ] [ text editor ]
        , gtRows 5
            []
            [ viewExeBox "ACC" "0"
            , viewExeBox "BAK" "<0>"
            , viewExeBox "LAST" "N/A"
            , viewExeBox "MODE" "IDLE"
            , viewExeBox "IDLE" "0%"
            ]
        ]


viewExeNode : ( Addr, ExeNode ) -> Html msg
viewExeNode ( ( x, y ), exe ) =
    div
        [ gridAreaXY ( x, y - 1 )
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


viewFaultyNode : Addr -> Html msg
viewFaultyNode ( x, y ) =
    div
        [ displayGrid
        , gridAreaXY ( x, y - 1 )
        , placeContentCenter
        , sOutline ("1px solid " ++ "red")
        , fg "red"
        ]
        [ text "ERROR" ]



-- SIM NODE


type alias SimNode =
    Grid.Cell InputNode OutputNode ExeNode


simNodeIntents : SimNode -> List Intent
simNodeIntents node =
    case node of
        In _ _ ->
            [ Write Down ]

        Out _ _ ->
            [ Read Up ]

        Exe exe ->
            ExeNode.intents exe

        Flt ->
            []


simNodeActions : SimNode -> List Action
simNodeActions node =
    case node of
        Out _ _ ->
            []

        _ ->
            case simNodeState node of
                S.ReadyToRun _ ->
                    []

                S.ReadBlocked dir4 _ ->
                    [ Reading dir4 ]

                S.WriteBlocked num dir4 _ ->
                    [ Writing dir4 num ]

                S.Done ->
                    []


simNodeState : SimNode -> NodeState SimNode
simNodeState node =
    case node of
        In conf inputNode ->
            InputNode.state inputNode |> S.map (In conf)

        Out conf outputNode ->
            OutputNode.state outputNode |> S.map (Out conf)

        Exe exeNode ->
            ExeNode.state exeNode |> S.map Exe

        Flt ->
            S.Done


viewSimNode : ( Addr, SimNode ) -> Html msg
viewSimNode ( addr, node ) =
    case node of
        In conf _ ->
            viewInputNode conf

        Out conf _ ->
            viewOutputNode conf

        Exe exe ->
            viewExeNode ( addr, exe )

        Flt ->
            viewFaultyNode addr



-- SIM


type alias Sim =
    { store : SimStore
    , state : SimState
    , cycle : Int
    }


type alias SimStore =
    Grid.Grid InputNode OutputNode ExeNode


type SimState
    = Stepping StepMode
    | Completed


type StepMode
    = Manual
    | Auto
    | AutoFast


initSim : Puzzle -> List ( Addr, ExeNode ) -> StepMode -> Sim
initSim puzzle exs stepMode =
    { store = initSimStore puzzle |> Grid.replaceExeEntries exs
    , state = Stepping stepMode
    , cycle = 0
    }


initSimStore : Puzzle -> SimStore
initSimStore puzzle =
    Grid.init puzzle
        (\conf -> InputNode.fromList conf.nums)
        (\conf -> OutputNode.fromExpected (List.length conf.nums))
        ExeNode.empty


simIntentsAndActions : SimStore -> ( List ( Addr, Intent ), List ( Addr, Action ) )
simIntentsAndActions simStore =
    Dict.foldl
        (\addr node ( intents, actions ) ->
            ( List.map (pair addr) (simNodeIntents node) ++ intents
            , List.map (pair addr) (simNodeActions node) ++ actions
            )
        )
        ( [], [] )
        simStore



-- SIM UPDATE


simSetStepMode : StepMode -> Sim -> Sim
simSetStepMode stepMode sim =
    case sim.state of
        Stepping _ ->
            { sim | state = Stepping stepMode }

        Completed ->
            sim


manualStepSim : Sim -> Sim
manualStepSim sim =
    sim
        |> simSetStepMode Manual
        |> autoStepSim


autoStepSim : Sim -> Sim
autoStepSim sim =
    let
        newStore =
            stepSimStore sim.store
    in
    if newStore == sim.store then
        { sim | state = Completed }

    else
        { sim
            | store = newStore
            , cycle = sim.cycle + 1
        }


stepSimStore : SimStore -> SimStore
stepSimStore store =
    Dict.foldl stepNode (initAcc store) store
        |> resolveAllReadBlocked
        |> resolveAllWriteBlocked


stepNode : Addr -> SimNode -> Acc -> Acc
stepNode addr node =
    case simNodeState node of
        S.WriteBlocked num dir cont ->
            addToWriteBlocked addr node num dir cont

        S.Done ->
            addToCompleted addr node

        S.ReadBlocked dir cont ->
            addToReadBlocked addr node dir cont

        S.ReadyToRun cont ->
            resolveAfterRun addr (cont ())


resolveAfterRun : Addr -> SimNode -> Acc -> Acc
resolveAfterRun addr node =
    case simNodeState node of
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


resolveAllWriteBlocked : WriteBlockedAcc a -> SimStore
resolveAllWriteBlocked acc =
    Dict.foldl (\addr { node } -> replaceEntry ( addr, node )) acc.completed acc.writeBlocked


type alias Acc =
    { readBlocked : ReadBlockedStore
    , writeBlocked : WriteBlockedStore
    , completed : SimStore
    }


type alias WriteBlockedAcc a =
    { a
        | writeBlocked : WriteBlockedStore
        , completed : SimStore
    }


type alias ReadBlockedStore =
    Dict Addr ReadBlockedNode


type alias ReadBlockedNode =
    ( SimNode, Dir4, Num -> SimNode )


type alias WriteBlockedStore =
    Dict Addr WriteBlockedNode


type alias WriteBlockedNode =
    { node : SimNode, num : Num, dir : Dir4, cont : () -> SimNode }


initAcc : SimStore -> Acc
initAcc store =
    { readBlocked = Dict.empty
    , writeBlocked = Dict.empty
    , completed = store
    }


addToReadBlocked :
    Addr
    -> SimNode
    -> Dir4
    -> (Num -> SimNode)
    -> { a | readBlocked : ReadBlockedStore }
    -> { a | readBlocked : ReadBlockedStore }
addToReadBlocked addr node dir cont acc =
    { acc | readBlocked = Dict.insert addr ( node, dir, cont ) acc.readBlocked }


addToWriteBlocked :
    Addr
    -> SimNode
    -> Num
    -> Dir4
    -> (() -> SimNode)
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
    -> SimNode
    -> { a | completed : SimStore }
    -> { a | completed : SimStore }
addToCompleted na n acc =
    { acc | completed = replaceEntry ( na, n ) acc.completed }


completeWriteBlocked : Addr -> SimNode -> WriteBlockedAcc a -> WriteBlockedAcc a
completeWriteBlocked addr node acc =
    { acc | writeBlocked = Dict.remove addr acc.writeBlocked }
        |> addToCompleted addr node



-- UI HELPERS


lightGray =
    UI.lightGray


lightOutline =
    UI.lightOutline
