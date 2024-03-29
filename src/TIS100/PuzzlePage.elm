module TIS100.PuzzlePage exposing
    ( Model
    , Msg
    , init
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
import TIS100.Effect as Eff exposing (Effect, autoFocus, returnToSegmentList, save, withEff, withEffBy, withoutEff)
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


init : Puzzle.Id -> List ( Addr, String ) -> Model
init id =
    initHelp (Puzzle.fromId id)


initHelp : Puzzle -> List ( Addr, String ) -> Model
initHelp puzzle sourceEntries =
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
    | ReturnToSegmentList


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
        (keyEventDecoder |> JD.andThen (keyEventToMsgDecoder model))
    ]
        |> Sub.batch


keyEventToMsgDecoder : Model -> KeyEvent -> JD.Decoder Msg
keyEventToMsgDecoder model ke =
    case model.state of
        Dialog _ _ ->
            if matchesNoModifiers [ "Escape" ] ke || matchesAlt [ "`" ] ke then
                JD.succeed CloseClicked

            else
                JD.fail ""

        TestPassed _ ->
            if
                matchesNoModifiers [ "Escape" ] ke
                    || matchesAlt [ "`" ] ke
            then
                JD.succeed CloseClicked

            else
                JD.fail ""

        Edit ->
            if matchesAlt [ "F1" ] ke then
                JD.succeed (OpenDialogClicked QuickRefDialog)

            else if
                matchesNoModifiers [ "Escape" ] ke
                    || matchesAlt [ "`" ] ke
            then
                JD.succeed (OpenDialogClicked SystemDialog)

            else
                (if matchesAlt [ "1" ] ke then
                    JD.succeed (StartDebugging Manual)

                 else if matchesAlt [ "2" ] ke then
                    JD.succeed (StartDebugging (Auto Normal))

                 else if matchesAlt [ "3" ] ke then
                    JD.succeed (StartDebugging (Auto Fast))

                 else
                    JD.fail ""
                )
                    |> JD.map EditMsg

        SIM _ _ ->
            if matchesAlt [ "F1" ] ke then
                JD.succeed (OpenDialogClicked QuickRefDialog)

            else
                (if
                    matchesNoModifiers [ "Escape" ] ke
                        || matchesAlt [ "`" ] ke
                 then
                    JD.succeed StopClicked

                 else if matchesAlt [ "1" ] ke then
                    JD.succeed StepOrPauseClicked

                 else if matchesAlt [ "2" ] ke then
                    JD.succeed (RunClicked Normal)

                 else if matchesAlt [ "3" ] ke then
                    JD.succeed (RunClicked Fast)

                 else
                    JD.fail ""
                )
                    |> JD.map SimMsg


type alias ExeDict =
    Dict Addr ExeNode


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        CloseClicked ->
            case model.state of
                TestPassed _ ->
                    { model | state = Edit } |> withoutEff

                Dialog _ dialogBG ->
                    case dialogBG of
                        EditBG ->
                            { model | state = Edit } |> withoutEff

                        SimBG stepMode sim ->
                            { model | state = SIM stepMode sim } |> withoutEff

                _ ->
                    model |> withoutEff

        OpenDialogClicked dialog ->
            case model.state of
                Edit ->
                    { model | state = Dialog dialog EditBG }
                        |> withEff autoFocus

                SIM stepMode sim ->
                    { model | state = Dialog dialog (SimBG stepMode sim) }
                        |> withEff autoFocus

                _ ->
                    model |> withoutEff

        ReturnToSegmentList ->
            model |> withEff returnToSegmentList

        EditMsg editMsg ->
            case model.state of
                Edit ->
                    updateWhenEditing editMsg model

                _ ->
                    model |> withoutEff

        SimMsg simMsg ->
            case model.state of
                SIM stepMode sim ->
                    updateWhenSimulating simMsg stepMode sim
                        |> mapFirst (\state -> { model | state = state })

                _ ->
                    model |> withoutEff


saveEff : Model -> Effect
saveEff model =
    save (Puzzle.id model.puzzle) (Dict.toList model.editors)


updateWhenEditing : EditMsg -> Model -> ( Model, Effect )
updateWhenEditing msg model =
    case msg of
        OnEditorInput addr string ->
            let
                editors =
                    replaceEntry ( addr, string ) model.editors
            in
            { model | editors = editors } |> withEffBy saveEff

        StartDebugging stepMode ->
            case maybeTraverseValues ExeNode.compile model.editors of
                Just exd ->
                    { model | state = initSim model.puzzle exd stepMode }
                        |> withoutEff

                Nothing ->
                    model |> withoutEff


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
        , ffMonospace
        , ttu
        , bold
        , pa "2ch"
        , gap "2ch"
        , positionRelative
        ]
        [ viewCycle model
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

        TestPassed sim ->
            viewTestPassedDialog model.puzzle sim

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
            , btnAutoFocus "return to segment list" ReturnToSegmentList
            , btn "close" CloseClicked
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


viewTestPassedDialog : Puzzle -> Sim -> Html Msg
viewTestPassedDialog puzzle sim =
    let
        stats =
            SimStore.stats sim.store
    in
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
            [ div [ tac ]
                [ text "- "
                , text (Puzzle.title puzzle)
                , text " - Test Passed -"
                ]
            , div
                [ gap "1ch"
                , displayGrid
                , placeContentCenter
                , gridTemplateColumns "1fr 1fr"
                ]
                [ div [ tar ] [ text "Cycles :" ]
                , div [] [ text (fromInt sim.cycle) ]
                , div [ tar ] [ text "Nodes :" ]
                , div [] [ text (fromInt stats.exeNodesUsed) ]
                , div [ tar ] [ text "instr :" ]
                , div [] [ text (fromInt stats.instructionCount) ]
                ]
            , fRow [ gap "2ch" ]
                [ btn "continue editing this segment" CloseClicked
                , btnAutoFocus "return to segment list" ReturnToSegmentList
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
        ([ UI.outlineNormal
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
                (SimStore.leftBarViewModel puzzle sim.store)
                |> Html.map SimMsg

        TestPassed sim ->
            LB.view
                { stop = Nothing
                , step = Nothing
                , run = Nothing
                , fast = Nothing
                }
                (SimStore.leftBarViewModel puzzle sim.store)

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
                        (SimStore.leftBarViewModel puzzle sim.store)


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
        [ div [ tac, UI.fgNormal ]
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
        [ div [ tac, UI.fgNormal ]
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
        , UI.outlineNormal
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
        , UI.outlineNormal
        , pa "0.5ch"
        , UI.fgNormal

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
        , UI.outlineNormal
        , displayGrid
        , gridTemplateColumns "18ch auto"
        ]
        [ viewSrcCodeWithSelection vm.srcCode vm.mbCurrentRow
        , viewExeBoxes vm
        ]


viewSrcCodeWithSelection : String -> Maybe Int -> Html.Html msg
viewSrcCodeWithSelection srcCode currentRow =
    Html.pre [ pa "0.5ch 0" ]
        (String.lines srcCode
            |> List.indexedMap
                (\i l ->
                    let
                        styles =
                            if Just (i + 1) == currentRow then
                                UI.highlightBright

                            else
                                [ UI.fgNormal ]
                    in
                    div (pl "0.5ch" :: styles) [ text (l ++ "\n") ]
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
        , UI.outlineNormal
        ]
        [ div [ UI.fgNormal ] [ text a ]
        , div [ UI.fgBright ] [ text b ]
        ]


viewFaultyNode : Addr -> Html msg
viewFaultyNode addr =
    div
        [ displayGrid
        , Addr.toGridArea addr
        , placeContentCenter
        , UI.outlineError
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
