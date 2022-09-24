module TIS100.PuzzlePage exposing
    ( Model
    , Msg
    , sampleModel
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Dict exposing (Dict)
import Html exposing (pre)
import Html.Attributes as HA
import Json.Decode as JD
import TIS100.Addr as Addr exposing (Addr)
import TIS100.Effect as Eff exposing (Effect, autoFocus, withEff, withoutEff)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
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
    , dialog : Maybe Dialog
    , editors : Editors
    , state : State
    }


type State
    = Edit
    | SIM StepMode Sim
    | TestPassed Sim
    | Dialog Dialog DialogBG


type DialogBG
    = EditBG
    | SimBG StepMode Sim


init : Puzzle -> List ( Addr, String ) -> Model
init puzzle sourceEntries =
    { puzzle = puzzle
    , dialog = Nothing
    , editors = initEditors puzzle |> replaceEntries sourceEntries
    , state = Edit
    }


type Msg
    = EditMsg EditMsg
    | SimMsg SimMsg
    | OpenDialogClicked Dialog
    | CloseClicked
    | OnKeyDown KeyEvent


type EditMsg
    = StartDebugging StepMode
    | OnEditorInput Addr String


type SimMsg
    = StopClicked
    | StepOrPauseClicked
    | RunClicked Speed
    | AutoStepTriggered


subscriptions : Model -> Sub Msg
subscriptions model =
    [ case model.state of
        SIM stepMode _ ->
            Sub.map SimMsg <|
                case stepMode of
                    Manual ->
                        Sub.none

                    Auto Normal ->
                        Time.every 20 (\_ -> AutoStepTriggered)

                    Auto Fast ->
                        Time.every 0 (\_ -> AutoStepTriggered)

        _ ->
            Sub.none
    , Browser.Events.onKeyDown
        (keyEventDecoder
            |> JD.andThen
                (\ke ->
                    let
                        _ =
                            Debug.log "Debug: " ke
                    in
                    case model.state of
                        Dialog _ _ ->
                            if matchesNoModifiers [ "Escape" ] ke then
                                JD.succeed CloseClicked

                            else
                                JD.fail ""

                        TestPassed _ ->
                            if matchesNoModifiers [ "Escape" ] ke then
                                JD.succeed CloseClicked

                            else
                                JD.fail ""

                        Edit ->
                            (if matchesCtrlAlt [ "w" ] ke then
                                JD.succeed (StartDebugging Manual)

                             else if matchesCtrlAlt [ "e" ] ke then
                                JD.succeed (StartDebugging (Auto Normal))

                             else if matchesCtrlAlt [ "r" ] ke then
                                JD.succeed (StartDebugging (Auto Fast))

                             else
                                JD.fail ""
                            )
                                |> JD.map EditMsg

                        _ ->
                            JD.fail ""
                )
        )
    ]
        |> Sub.batch


type alias ExeDict =
    Dict Addr ExeNode


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case ( msg, model.state ) of
        ( CloseClicked, Dialog _ bg ) ->
            case bg of
                EditBG ->
                    { model | state = Edit } |> withoutEff

                SimBG stepMode sim ->
                    { model | state = SIM stepMode sim } |> withoutEff

        ( CloseClicked, TestPassed _ ) ->
            { model | state = Edit } |> withoutEff

        ( OpenDialogClicked dialog, Edit ) ->
            { model | state = Dialog dialog EditBG }
                |> withEff autoFocus

        ( OpenDialogClicked dialog, SIM stepMode sim ) ->
            { model | state = Dialog dialog (SimBG stepMode sim) }
                |> withEff autoFocus

        ( EditMsg editMsg, Edit ) ->
            updateWhenEditing editMsg model |> withoutEff

        ( SimMsg simMsg, SIM stepMode sim ) ->
            updateWhenSimulating simMsg stepMode sim
                |> mapFirst (\state -> { model | state = state })

        _ ->
            withoutEff model


updateWhenEditing : EditMsg -> Model -> Model
updateWhenEditing msg model =
    case msg of
        OnEditorInput addr string ->
            { model
                | editors =
                    replaceEntry ( addr, string ) model.editors
            }

        StartDebugging stepMode ->
            case maybeTraverseValues ExeNode.compile model.editors of
                Just exd ->
                    { model | state = initSim model.puzzle exd stepMode }

                Nothing ->
                    model


updateWhenSimulating : SimMsg -> StepMode -> Sim -> ( State, Effect )
updateWhenSimulating msg stepMode sim =
    case msg of
        StopClicked ->
            Edit |> withoutEff

        StepOrPauseClicked ->
            case stepMode of
                Manual ->
                    step sim |> updateOnStepResponse stepMode

                Auto _ ->
                    SIM Manual sim |> withoutEff

        RunClicked speed ->
            SIM (Auto speed) sim |> withoutEff

        AutoStepTriggered ->
            case stepMode of
                Manual ->
                    SIM stepMode sim |> withoutEff

                Auto speed ->
                    autoStep speed sim |> updateOnStepResponse stepMode


updateOnStepResponse : StepMode -> StepResponse -> ( State, Effect )
updateOnStepResponse stepMode stepRes =
    case stepRes of
        Completed sim2 ->
            TestPassed sim2 |> withEff autoFocus

        Pending sim2 ->
            SIM stepMode sim2 |> withoutEff


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
        , button [ notifyClick (OpenDialogClicked QuickRefDialog) ] [ text "quick ref" ]
        , button [ notifyClick (OpenDialogClicked SystemDialog) ] [ text "system" ]
        , fRow [ gap "2ch" ] [ viewLeftBar model, viewGrid model ]
        , viewDialog model
        ]


type Dialog
    = SystemDialog
    | QuickRefDialog


viewDialog : Model -> Html Msg
viewDialog model =
    case model.state of
        Dialog QuickRefDialog _ ->
            viewQuickRefDialog

        Dialog SystemDialog _ ->
            viewSystemDialog

        TestPassed _ ->
            viewTestPassedDialog

        _ ->
            noView


viewSystemDialog : Html Msg
viewSystemDialog =
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
            [ text "system dialog"
            , btnAutoFocus "close" CloseClicked
            ]
        ]


viewQuickRefDialog : Html Msg
viewQuickRefDialog =
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
            [ text "quick ref"
            , btnAutoFocus "close" CloseClicked
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
                [ btnAutoFocus "continue editing this segment" CloseClicked
                , btn "return to segment list" CloseClicked
                ]
            ]
        ]


btnAutoFocus : String -> msg -> Html msg
btnAutoFocus =
    btnHelp [ Eff.attrAutoFocusId, autofocus True ]


btn : String -> msg -> Html msg
btn =
    btnHelp []


btnHelp : List (Attribute msg) -> String -> msg -> Html msg
btnHelp attrs txt msg =
    button
        ([ UI.lightOutline
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
            ++ attrs
        )
        [ text txt ]


viewCycle : Model -> Html msg
viewCycle model =
    let
        cycleText =
            case model.state of
                Edit ->
                    "NA"

                SIM _ sim ->
                    fromInt sim.cycle

                TestPassed sim ->
                    fromInt sim.cycle

                Dialog _ EditBG ->
                    "NA"

                Dialog _ (SimBG _ sim) ->
                    fromInt sim.cycle
    in
    div [] [ text "Cycle: ", text cycleText ]


viewLeftBar : Model -> Html Msg
viewLeftBar { puzzle, state } =
    case state of
        Edit ->
            LB.view
                { stop = Nothing
                , step = Just (StartDebugging Manual)
                , run = Just (StartDebugging (Auto Normal))
                , fast = Just (StartDebugging (Auto Fast))
                }
                (Puzzle.leftBarViewModel puzzle)
                |> Html.map EditMsg

        SIM _ sim ->
            LB.view
                { stop = Just StopClicked
                , step = Just StepOrPauseClicked
                , run = Just (RunClicked Normal)
                , fast = Just (RunClicked Fast)
                }
                (SimStore.leftBarViewModel sim.store)
                |> Html.map SimMsg

        TestPassed sim ->
            LB.view
                { stop = Nothing
                , step = Nothing
                , run = Nothing
                , fast = Nothing
                }
                (SimStore.leftBarViewModel sim.store)

        Dialog _ bg ->
            case bg of
                EditBG ->
                    LB.view
                        { stop = Nothing
                        , step = Nothing
                        , run = Nothing
                        , fast = Nothing
                        }
                        (Puzzle.leftBarViewModel puzzle)

                SimBG _ sim ->
                    LB.view
                        { stop = Nothing
                        , step = Nothing
                        , run = Nothing
                        , fast = Nothing
                        }
                        (SimStore.leftBarViewModel sim.store)


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

            SIM _ sim ->
                viewSimGridItems puzzle sim

            TestPassed sim ->
                viewSimGridItems puzzle sim

            Dialog _ EditBG ->
                viewEditModeGridItems puzzle editors

            Dialog _ (SimBG _ sim) ->
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


viewEditor : Addr -> Editor -> Html EditMsg
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
        , HA.spellcheck False
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


viewExeNode : Addr -> ExeNode.ViewModel -> Html msg
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
        |> Dict.union
            (Dict.map viewEditor editors
                |> Dict.map (\_ -> Html.map EditMsg)
            )
        |> Dict.values



-- SIM


type alias Sim =
    { store : SimStore.Model
    , cycle : Int
    }


type StepMode
    = Manual
    | Auto Speed


type Speed
    = Normal
    | Fast


initSim : Puzzle -> ExeDict -> StepMode -> State
initSim puzzle exd stepMode =
    SIM
        stepMode
        { store = SimStore.init puzzle exd
        , cycle = 0
        }


autoStep : Speed -> Sim -> StepResponse
autoStep speed =
    case speed of
        Normal ->
            step

        Fast ->
            autoStepFast


autoStepFast : Sim -> StepResponse
autoStepFast =
    let
        autoStepFastHelp n sim =
            case step sim of
                Pending sim2 ->
                    autoStepFastHelp (n - 1) sim2

                Completed sim2 ->
                    Completed sim2
    in
    autoStepFastHelp 15


type StepResponse
    = Completed Sim
    | Pending Sim


step : Sim -> StepResponse
step sim =
    if SimStore.isCompleted sim.store then
        Completed sim

    else
        Pending
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
            viewExeNode addr (ExeNode.viewModel exe)

        SimStore.FLT ->
            viewFaultyNode addr
