module TIS100.PuzzlePage exposing
    ( Model
    , Msg
    , sampleModel
    , subscriptions
    , update
    , view
    )

import Dict exposing (Dict)
import Html
import TIS100.Addr as Addr exposing (Addr)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.NodeState as S exposing (NodeState)
import TIS100.Ports as Ports exposing (Action(..), Intent(..))
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.PuzzlePage.SimStore as SimStore exposing (SimNode(..), SimStore)
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
    , editors : Editors
    , exs : List ( Addr, ExeNode )
    , state : State
    }


type State
    = Edit
    | Sim_ Sim


init : Puzzle -> List ( Addr, ExeNode ) -> Model
init puzzle exs =
    let
        sourceEntries =
            List.map (mapSecond ExeNode.toSource) exs
    in
    { puzzle = puzzle
    , editors = initEditorsWithSource puzzle sourceEntries
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


leftBarViewModel : Model -> LB.ViewModel
leftBarViewModel { puzzle, state } =
    case state of
        Edit ->
            { inputs = editModeInputColumns puzzle
            , outputs = editModeOutputColumns puzzle
            }

        Sim_ sim ->
            { inputs = SimStore.leftBarInputs sim.store
            , outputs = SimStore.leftBarOutputs sim.store
            }


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
            [ LB.view
                { step = STEP
                , stop = STOP
                , run = RUN
                , fast = FAST
                }
                (leftBarViewModel model)
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
                viewEditModeGridItems puzzle editors

            Sim_ sim ->
                viewSimGridItems puzzle sim
        )



-- NODE


viewInputNode : IOConfig -> Html msg
viewInputNode { x, title } =
    div
        [ displayGrid
        , placeContentCenter
        , sWidth "50%"
        , sHeight UI.gapSize
        , Addr.inputGridArea x
        , positionRelative
        , style "bottom" UI.gapSize
        ]
        [ div [ tac, fg UI.lightGray ]
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
        , Addr.outputGridArea x
        , positionRelative
        , top100
        ]
        [ div [ tac, fg UI.lightGray ]
            [ div [] [ text title ]
            ]
        ]


viewEditor : ( Addr, Editor ) -> Html msg
viewEditor ( addr, editor ) =
    div
        [ Addr.toGridArea addr
        , UI.lightOutline
        , dGrid
        , gridAutoFlowColumn
        ]
        [ Html.textarea
            [ sWidth "18ch"
            , pa "1ch"

            -- reset
            , ma ""
            , borderNone
            , sOutline "none"
            , style "resize" "none"

            -- inherit
            , bgc "inherit"
            , fg "inherit"
            , textTransform "inherit"
            , style "font" "inherit"
            ]
            [ text editor ]
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
viewExeNode ( addr, exe ) =
    div
        [ Addr.toGridArea addr
        , UI.lightOutline
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
    div [ dGrid, tac, placeContentCenter, sOutline ("1px solid " ++ UI.lightGray) ]
        [ div [ fg UI.lightGray ] [ text a ]
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
viewFaultyNode addr =
    div
        [ displayGrid
        , Addr.toGridArea addr
        , placeContentCenter
        , sOutline ("1px solid " ++ "red")
        , fg "red"
        ]
        [ text "ERROR" ]



-- EDIT MODE


type alias Editors =
    Dict Addr Editor


type alias Editor =
    String


initEditorsWithSource : Puzzle -> List ( Addr, String ) -> Dict Addr String
initEditorsWithSource puzzle sourceEntries =
    replaceEntries sourceEntries (initEditors puzzle)


initEditors : Puzzle -> Editors
initEditors puzzle =
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


editModeInputColumns : Puzzle -> List LB.Input
editModeInputColumns puzzle =
    List.map
        (\conf ->
            { title = conf.title
            , nums = SelectionList.None conf.nums
            }
        )
        puzzle.inputs


editModeOutputColumns : Puzzle -> List LB.Output
editModeOutputColumns puzzle =
    List.map
        (\conf ->
            { title = conf.title
            , expected = SelectionList.None conf.nums
            , actual = []
            }
        )
        puzzle.outputs


viewEditModeGridItems : Puzzle -> Dict Addr Editor -> List (Html msg)
viewEditModeGridItems puzzle editors =
    viewEditModeNodes puzzle editors
        ++ Ports.viewAllPorts puzzle


viewEditModeNodes : Puzzle -> Dict Addr Editor -> List (Html msg)
viewEditModeNodes puzzle editors =
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



-- SIM


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


type SimState
    = Stepping StepMode
    | Completed


type StepMode
    = Manual
    | Auto
    | AutoFast


initSim : Puzzle -> List ( Addr, ExeNode ) -> StepMode -> Sim
initSim puzzle exs stepMode =
    { store = SimStore.init puzzle exs
    , state = Stepping stepMode
    , cycle = 0
    }


viewSimGridItems : Puzzle -> Sim -> List (Html msg)
viewSimGridItems puzzle { store } =
    List.map viewSimNode (Dict.toList store)
        ++ Ports.view puzzle (SimStore.portsViewModel store)



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
            SimStore.step sim.store
    in
    if newStore == sim.store then
        { sim | state = Completed }

    else
        { sim
            | store = newStore
            , cycle = sim.cycle + 1
        }
