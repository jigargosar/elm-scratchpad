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
import TIS100.NodeState as NS exposing (NodeState)
import TIS100.Ports as Ports exposing (Action(..), Intent(..))
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.PuzzlePage.SimStore as SimStore
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
    | OnEditorInput Addr String


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
    case compile model.editors of
        Just exs ->
            { model | state = Sim_ (initSim model.puzzle exs stepMode) }

        Nothing ->
            model


compile : Editors -> Maybe (List ( Addr, ExeNode ))
compile editors =
    editors
        |> Dict.toList
        |> List.filterMap
            (\( a, sc ) -> ExeNode.compile sc |> Maybe.map (pair a))
        |> Just
        |> maybeFilter (List.length >> eq (Dict.size editors))


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

                OnEditorInput addr string ->
                    { model | editors = replaceEntry ( addr, string ) model.editors }

        Sim_ sim ->
            case msg of
                STOP ->
                    startEditing model

                STEP ->
                    { model | state = Sim_ (manualStep sim) }

                AutoStep ->
                    { model | state = Sim_ (autoStep sim) }

                RUN ->
                    { model | state = Sim_ (setStepMode Auto sim) }

                FAST ->
                    { model | state = Sim_ (setStepMode AutoFast sim) }

                OnEditorInput addr string ->
                    model


leftBarViewModel : Model -> LB.ViewModel
leftBarViewModel { puzzle, state } =
    case state of
        Edit ->
            { inputs = editModeInputColumns puzzle
            , outputs = editModeOutputColumns puzzle
            }

        Sim_ sim ->
            SimStore.leftBarViewModel sim.store


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


viewGrid : Model -> Html Msg
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


viewEditor : ( Addr, Editor ) -> Html Msg
viewEditor ( addr, editor ) =
    div
        [ Addr.toGridArea addr
        , UI.lightOutline
        , displayGrid
        , gridTemplateColumns "18ch auto"
        ]
        [ Html.textarea
            [ pa "1ch"

            -- reset
            , borderNone
            , outlineNone
            , resizeNone

            -- inherit
            , bgcInherit
            , fgInherit
            , ttInherit
            , fontInherit
            , onInput (OnEditorInput addr)
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
        , displayGrid
        , gridTemplateColumns "18ch auto"
        ]
        [ div [ pa "1ch" ] [ text (ExeNode.toSource exe) ]
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
    div [ displayGrid, tac, placeContentCenter, sOutline ("1px solid " ++ UI.lightGray) ]
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
        NS.ReadyToRun _ ->
            "RUN"

        NS.ReadBlocked _ _ ->
            "READ"

        NS.WriteBlocked _ _ _ ->
            writeModeLabel

        NS.Done ->
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


viewEditModeGridItems : Puzzle -> Dict Addr Editor -> List (Html Msg)
viewEditModeGridItems puzzle editors =
    viewEditModeNodes puzzle editors
        ++ Ports.viewAllPorts puzzle


viewEditModeNodes : Puzzle -> Dict Addr Editor -> List (Html Msg)
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


type alias Sim =
    { store : SimStore.Model
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


setStepMode : StepMode -> Sim -> Sim
setStepMode stepMode sim =
    case sim.state of
        Stepping _ ->
            { sim | state = Stepping stepMode }

        Completed ->
            sim


manualStep : Sim -> Sim
manualStep sim =
    sim
        |> setStepMode Manual
        |> autoStep


autoStep : Sim -> Sim
autoStep sim =
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


viewSimGridItems : Puzzle -> Sim -> List (Html msg)
viewSimGridItems puzzle { store } =
    List.map viewSimNode (Dict.toList store)
        ++ Ports.view puzzle (SimStore.portsViewModel store)


viewSimNode : ( Addr, SimStore.Node ) -> Html msg
viewSimNode ( addr, node ) =
    case node of
        SimStore.In conf _ ->
            viewInputNode conf

        SimStore.Out conf _ ->
            viewOutputNode conf

        SimStore.Exe exe ->
            viewExeNode ( addr, exe )

        SimStore.Flt ->
            viewFaultyNode addr
