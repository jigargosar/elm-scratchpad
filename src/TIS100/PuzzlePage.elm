module TIS100.PuzzlePage exposing
    ( Model
    , Msg
    , sampleModel
    , subscriptions
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (pre)
import Html.Attributes
import TIS100.Addr as Addr exposing (Addr)
import TIS100.Exe as Exe exposing (ExeNode)
import TIS100.Num as Num exposing (Num)
import TIS100.Ports as Ports exposing (Action(..), Intent(..))
import TIS100.Puzzle as Puzzle exposing (InConfig, OutConfig, Puzzle)
import TIS100.PuzzlePage.Compiler as Compiler exposing (ErrorDetail)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.PuzzlePage.SimStore as SimStore
import TIS100.UI as UI
import Time
import Utils exposing (..)



-- MAIN


sampleModel : Model
sampleModel =
    let
        sourceEntries : List ( Addr, String )
        sourceEntries =
            [ ( ( 0, 1 ), "MOV UP DOWN" )
            , ( ( 0, 2 ), "MOV UP DOWN" )
            , ( ( 0, 3 ), "MOV UP right" )
            , ( ( 1, 1 ), "" )
            , ( ( 1, 2 ), "" )
            , ( ( 1, 3 )
              , [ "S: MOV LEFT ACC"
                , "MOV ACC RIGHT"
                , ""
                , "JGZ 1"
                , "MOV 0 DOWN"
                , "JMP S"
                , ""
                , "1: MOV 1 DOWN"
                ]
                    |> String.join "\n"
              )
            , ( ( 2, 1 ), "" )
            , ( ( 2, 2 ), "" )
            , ( ( 2, 3 )
              , [ "S: MOV LEFT ACC"
                , "MOV ACC RIGHT"
                , ""
                , "JEZ 1"
                , "MOV 0 DOWN"
                , "JMP S"
                , ""
                , "1: MOV 1 DOWN"
                ]
                    |> String.join "\n"
              )
            , ( ( 3, 1 ), "" )
            , ( ( 3, 2 ), "" )
            , ( ( 3, 3 )
              , [ "S: MOV LEFT ACC"
                , "# MOV ACC RIGHT"
                , ""
                , "JLZ 1"
                , "MOV 0 DOWN"
                , "JMP S"
                , ""
                , "1: MOV 1 DOWN"
                ]
                    |> String.join "\n"
              )
            ]
    in
    init Puzzle.samplePuzzle2 sourceEntries



--noinspection ElmUnusedSymbol


sampleModel1 : Model
sampleModel1 =
    let
        sourceEntries : List ( Addr, String )
        sourceEntries =
            [ ( ( 0, 1 ), "mov up acc\n\n\nmov acc down" )
            , ( ( 0, 2 ), "Mov up down\nmov 1 acc" )
            , ( ( 0, 3 ), "Mov up down\nnop" )
            , ( ( 1, 1 ), "Mov up down" )
            , ( ( 1, 2 ), "Mov up down" )
            , ( ( 1, 3 ), "Mov up down" )
            , ( ( 2, 1 ), "lbl:Jmp lbl\n jmp : \na : Jmp : " )
            , ( ( 2, 2 ), "Mov up down" )
            , ( ( 2, 3 ), "Mov up down" )
            , ( ( 3, 1 ), "Mov up down" )
            , ( ( 3, 2 ), "Mov up down" )
            , ( ( 3, 3 ), "Mov up down" )
            ]
    in
    init Puzzle.samplePuzzle1 sourceEntries



-- MODEL


type alias Model =
    { puzzle : Puzzle
    , editors : Editors
    , state : State
    }


type State
    = Edit Dialog
    | SIM Dialog Sim
    | TestPassed Sim


init : Puzzle -> List ( Addr, String ) -> Model
init puzzle sourceEntries =
    { puzzle = puzzle
    , editors =
        initEditors puzzle
            |> replaceEntries sourceEntries
    , state = Edit NoDialog
    }


type Msg
    = STOP
    | STEP
    | RUN
    | FAST
    | AutoStep
    | AutoStepFast
    | OnEditorInput Addr String
    | OnContinueEdit


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Edit _ ->
            Sub.none

        SIM _ sim ->
            case sim.state of
                Stepping debugger ->
                    case debugger of
                        Manual ->
                            Sub.none

                        Auto ->
                            Time.every 20 (\_ -> AutoStep)

                        AutoFast ->
                            Time.every 0 (\_ -> AutoStepFast)

                Completed ->
                    Sub.none

        TestPassed _ ->
            Sub.none


startDebugging : StepMode -> Model -> Model
startDebugging stepMode model =
    case maybeMapValues Exe.compile model.editors of
        Just exs ->
            { model | state = SIM NoDialog (initSim model.puzzle exs stepMode) }

        Nothing ->
            model


type alias ExeDict =
    Dict Addr ExeNode


update : Msg -> Model -> Model
update msg model =
    case model.state of
        Edit NoDialog ->
            updateWhenEditing msg model

        Edit _ ->
            Debug.todo "todo"

        SIM NoDialog sim ->
            updateWhenSimulating msg sim model

        SIM _ _ ->
            Debug.todo "todo"

        TestPassed _ ->
            case msg of
                OnContinueEdit ->
                    { model | state = Edit NoDialog }

                _ ->
                    model


updateWhenSimulating : Msg -> Sim -> Model -> Model
updateWhenSimulating msg sim model =
    case msg of
        OnEditorInput _ _ ->
            model

        STOP ->
            { model | state = Edit NoDialog }

        OnContinueEdit ->
            { model | state = Edit NoDialog }

        STEP ->
            { model | state = manualStep sim }

        RUN ->
            { model | state = SIM NoDialog (setStepMode Auto sim) }

        FAST ->
            { model | state = SIM NoDialog (setStepMode AutoFast sim) }

        AutoStep ->
            { model | state = step sim }

        AutoStepFast ->
            { model | state = autoStepFast sim }


updateWhenEditing : Msg -> Model -> Model
updateWhenEditing msg model =
    case msg of
        STOP ->
            model

        STEP ->
            startDebugging Manual model

        RUN ->
            startDebugging Auto model

        FAST ->
            startDebugging AutoFast model

        OnEditorInput addr string ->
            { model
                | editors =
                    replaceEntry ( addr, string ) model.editors
            }

        AutoStep ->
            model

        AutoStepFast ->
            model

        OnContinueEdit ->
            model


leftBarViewModel : Model -> LB.ViewModel
leftBarViewModel { puzzle, state } =
    case state of
        Edit _ ->
            Puzzle.leftBarViewModel puzzle

        SIM _ sim ->
            SimStore.leftBarViewModel sim.store

        TestPassed sim ->
            SimStore.leftBarViewModel sim.store


view : Model -> Html Msg
view model =
    fCol
        [ sWidth "fit-content"
        , sHeight "fit-content"
        , fontSize "12px"
        , styleLineHeight "1"
        , pa "2ch"
        , bold
        , ffMonospace
        , gap "2ch"
        , ttu
        , positionRelative
        ]
        [ viewCycle model
        , fRow [ gap "2ch" ] [ viewLeftBar model, viewGrid model ]
        , viewDialog model
        ]


type Dialog
    = SystemDialog
    | HelpDialog
    | NoDialog


type DialogView
    = TestPassedDialogView
    | HelpDialogView
    | NoDialogView


toDialogView : Model -> DialogView
toDialogView model =
    case model.state of
        SIM dialog _ ->
            toDialogViewHelp dialog

        Edit dialog ->
            toDialogViewHelp dialog

        TestPassed _ ->
            TestPassedDialogView


toDialogViewHelp : Dialog -> DialogView
toDialogViewHelp dialog =
    case dialog of
        SystemDialog ->
            Debug.todo "todo"

        HelpDialog ->
            HelpDialogView

        NoDialog ->
            NoDialogView


viewDialog : Model -> Html Msg
viewDialog model =
    case toDialogView model of
        NoDialogView ->
            noView

        TestPassedDialogView ->
            viewTestPassedDialog

        HelpDialogView ->
            viewHelpDialog


viewHelpDialog : Html Msg
viewHelpDialog =
    div
        [ positionAbsolute
        , top0
        , left0
        , bottom0
        , right0
        , displayGrid
        , placeContentCenter
        , bgc (blackA 0.8)
        ]
        [ fCol
            [ style "border" "solid white"
            , style "border-width" "1.5ch 1ch"
            , pa "2ch"
            , gap "2ch"
            , bgc black
            ]
            [ text "help"
            , text "commands"
            , text "a"
            , text "b"
            , text "c"
            ]
        ]


viewTestPassedDialog : Html Msg
viewTestPassedDialog =
    div
        [ positionAbsolute
        , top0
        , left0
        , bottom0
        , right0
        , displayGrid
        , placeContentCenter
        , bgc (blackA 0.8)
        ]
        [ fCol
            [ style "border" "solid white"
            , style "border-width" "1.5ch 1ch"
            , pa "2ch"
            , gap "2ch"
            , bgc black
            ]
            [ div [ tac ] [ text "- signal comparator - Test Passed -" ]
            , fRow [ gap "2ch" ]
                [ btn "continue editing this segment" OnContinueEdit
                , btn "return to segment list" OnContinueEdit
                ]
            ]
        ]


btn : String -> msg -> Html msg
btn txt msg =
    button
        [ UI.lightOutline
        , bgc "inherit"
        , fg "inherit"
        , style "text-transform" "inherit"
        , style "font" "inherit"
        , borderNone
        , displayGrid
        , placeContentCenter
        , pa "1ch"
        , notifyClick msg
        ]
        [ text txt ]


viewCycle : Model -> Html msg
viewCycle model =
    let
        cycleText =
            case model.state of
                Edit _ ->
                    "NA"

                SIM _ sim ->
                    fromInt sim.cycle

                TestPassed sim ->
                    fromInt sim.cycle
    in
    div [] [ text "Cycle: ", text cycleText ]


viewLeftBar : Model -> Html Msg
viewLeftBar model =
    LB.view
        { step = STEP
        , stop = STOP
        , run = RUN
        , fast = FAST
        }
        (leftBarViewModel model)


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
            Edit _ ->
                viewEditModeGridItems puzzle editors

            SIM _ sim ->
                viewSimGridItems puzzle sim

            TestPassed sim ->
                viewSimGridItems puzzle sim
        )



-- NODE


viewInputNode : InConfig -> Html msg
viewInputNode c =
    div
        [ displayGrid
        , placeContentCenter
        , sWidth "50%"
        , sHeight UI.gapSize
        , Addr.inputGridArea <| Puzzle.inX c
        , positionRelative
        , style "bottom" UI.gapSize
        ]
        [ div [ tac, fg UI.lightGray ]
            [ div [] [ text <| Puzzle.inTitle c ]
            , div [] [ text "(IDLE 0%)" ]
            ]
        ]


viewOutputNode : OutConfig -> Html msg
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


viewEditor : Addr -> Editor -> Html Msg
viewEditor addr editor =
    let
        errors =
            Compiler.getErrorDetails editor

        headerView =
            case errors of
                [] ->
                    noView

                h :: _ ->
                    viewCompilerErrorMsg h.msg
    in
    div
        [ Addr.toGridArea addr
        , UI.lightOutline
        , displayGrid
        , gridTemplateColumns "18ch auto"
        , positionRelative
        ]
        [ headerView
        , viewErrorMarks errors
        , viewEditorTextArea (OnEditorInput addr) editor
        , viewExeBoxes { acc = Num.zero, mode = "IDLE" }
        ]


viewErrorMarks : List ErrorDetail -> Html.Html msg
viewErrorMarks errors =
    pre
        [ positionAbsolute
        , pa "0.5ch"
        , noPointerEvents
        , fg transparent
        ]
        (List.map viewErrorMark errors)


viewErrorMark : ErrorDetail -> Html msg
viewErrorMark error =
    span [ positionAbsolute ]
        [ text (String.repeat (error.row - 1) "\n")
        , text (String.repeat (error.startCol - 1) " ")
        , span
            [ textDecoration "underline 1px solid red"
            ]
            [ text (String.repeat (error.endCol - error.startCol + 1) " ") ]
        ]


viewEditorTextArea : (String -> msg) -> String -> Html.Html msg
viewEditorTextArea onInputMsg editor =
    Html.textarea
        [ -- reset
          borderNone
        , outlineNone
        , resizeNone

        -- inherit
        , bgcInherit
        , fgInherit
        , ttInherit
        , fontInherit

        -- actual
        , onInput onInputMsg
        , UI.lightOutline
        , pa "0.5ch"

        -- new
        , Html.Attributes.spellcheck False
        , whiteSpace "pre"
        , overflowClip
        , w100
        , h100
        ]
        [ text editor ]


viewCompilerErrorMsg : String -> Html msg
viewCompilerErrorMsg msg =
    div
        [ positionAbsolute
        , style "top" "-4ch"
        , w100
        , sHeight "3ch"
        , displayGrid
        , bgc "hsl(0deg 80% 50%)"
        , fg black
        , placeContentCenter
        ]
        [ text msg ]


viewExeNode : Addr -> Exe.ViewModel -> Html msg
viewExeNode addr vm =
    div
        [ Addr.toGridArea addr
        , UI.lightOutline
        , displayGrid
        , gridTemplateColumns "18ch auto"
        ]
        [ viewSrc vm.srcCode vm.mbCurrentRow
        , viewExeBoxes vm
        ]


viewSrc : String -> Maybe Int -> Html.Html msg
viewSrc srcCode currentRow =
    Html.pre [ pa "0.5ch 0" ]
        (String.lines srcCode
            |> List.indexedMap
                (\i l ->
                    if Just (i + 1) == currentRow then
                        div [ pl "0.5ch", fg black, bgc UI.lightGray ]
                            [ text (l ++ "\n") ]

                    else
                        div [ pl "0.5ch" ] [ text (l ++ "\n") ]
                )
        )


viewExeBoxes : { a | acc : Num, mode : String } -> Html msg
viewExeBoxes { acc, mode } =
    gtRows 5
        []
        [ viewExeBox "ACC" (Num.toString acc)
        , viewExeBox "BAK" "<0>"
        , viewExeBox "LAST" "N/A"
        , viewExeBox "MODE" mode
        , viewExeBox "IDLE" "0%"
        ]


viewExeBox : String -> String -> Html msg
viewExeBox a b =
    div
        [ displayGrid
        , tac
        , placeContentCenter
        , UI.lightOutline
        ]
        [ div [ fg UI.lightGray ] [ text a ]
        , div [] [ text b ]
        ]


viewFaultyNode : Addr -> Html msg
viewFaultyNode addr =
    div
        [ displayGrid
        , Addr.toGridArea addr
        , placeContentCenter
        , UI.errorOutline
        , UI.fgError
        ]
        [ text "ERROR" ]



-- EDIT MODE


type alias Editors =
    Dict Addr Editor


type alias Editor =
    String


initEditors : Puzzle -> Editors
initEditors puzzle =
    Puzzle.getExeAddr puzzle
        |> List.map (pairTo "")
        |> Dict.fromList


viewEditModeGridItems : Puzzle -> Editors -> List (Html Msg)
viewEditModeGridItems puzzle editors =
    Ports.viewAllPorts puzzle
        ++ viewEditModeNodes puzzle editors


viewEditModeNodes : Puzzle -> Editors -> List (Html Msg)
viewEditModeNodes puzzle editors =
    Puzzle.toDictBy
        { in_ = viewInputNode
        , out = viewOutputNode
        , exe = always noView
        , flt = viewFaultyNode
        }
        puzzle
        |> Dict.union (Dict.map viewEditor editors)
        |> Dict.values



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


initSim : Puzzle -> ExeDict -> StepMode -> Sim
initSim puzzle exd stepMode =
    { store = SimStore.init puzzle exd
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


manualStep : Sim -> State
manualStep sim =
    sim
        |> setStepMode Manual
        |> step


autoStepFast : Sim -> State
autoStepFast sim =
    autoStepFastHelp 15 sim


autoStepFastHelp n sim =
    case step sim of
        SIM _ sim2 ->
            autoStepFastHelp (n - 1) sim2

        x ->
            x


step : Sim -> State
step sim =
    if SimStore.isCompleted sim.store then
        TestPassed sim

    else
        SIM NoDialog
            { sim
                | store = SimStore.step sim.store
                , cycle = sim.cycle + 1
            }


viewSimGridItems : Puzzle -> Sim -> List (Html msg)
viewSimGridItems puzzle { store } =
    List.map viewSimNode (Dict.toList store)
        ++ Ports.view puzzle (SimStore.portsViewModel store)


viewSimNode : ( Addr, SimStore.Node ) -> Html msg
viewSimNode ( addr, node ) =
    case node of
        SimStore.IN conf _ ->
            viewInputNode conf

        SimStore.OUT conf _ ->
            viewOutputNode conf

        SimStore.EXE exe ->
            viewExeNode addr (Exe.viewModel exe)

        SimStore.FLT ->
            viewFaultyNode addr
