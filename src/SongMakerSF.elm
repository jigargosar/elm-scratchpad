port module SongMakerSF exposing (main)

import Browser.Dom
import Browser.Navigation exposing (Key)
import Html
import Html.Attributes as HA
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra
import Random
import Random.List
import Set exposing (Set)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Task
import Url exposing (Url)
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:
    Clone: Chrome Music Labs - Song Maker.

    # Next
    * [x] add all UI elements.
    * [x] make UI functional.
    * [x] implement features represented by UI.
        * [x] Instruments
        * [x] Tempo
        * [x] Settings
    * [x] Update player on tempo/settings change
    * Undo
    * Save




-}


port scheduleNote : Note -> Cmd msg


port onAudioContextTime : (Float -> msg) -> Sub msg


main =
    browserApplication
        { init = init
        , onUrlRequest = always NOP
        , onUrlChange = always NOP
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias PaintedPositions =
    Set Int2


type alias Model =
    { instrumentPositions : PaintedPositions
    , percussionPositions : PaintedPositions
    , stepIndex : Int
    , playState : PlayerState
    , drawState : Maybe ( Tool, GridType )
    , settingsDialog : Maybe Settings
    , settings : Settings
    , instrument : Instrument
    , percussion : Percussion
    , tempo : Int
    , audioTime : Float
    , key : Key
    }


type alias DataModel =
    { paintedPositions : PaintedPositions
    , instrumentPositions : PaintedPositions
    , percussionPositions : PaintedPositions
    , settings : Settings
    , instrument : Instrument
    , percussion : Percussion
    , tempo : Int
    }


type Instrument
    = Piano
    | Strings
    | Woodwind
    | Synth
    | Marimba


instrument1Name : Instrument -> String
instrument1Name i =
    case i of
        Piano ->
            "Piano"

        Strings ->
            "Strings"

        Woodwind ->
            "Woodwind"

        Synth ->
            "Synth"

        Marimba ->
            "Marimba"


cycleInstrument1 : Instrument -> Instrument
cycleInstrument1 i =
    case i of
        Piano ->
            Strings

        Strings ->
            Woodwind

        Woodwind ->
            Synth

        Synth ->
            Marimba

        Marimba ->
            Piano


type Percussion
    = Electronic
    | Blocks
    | Kit
    | Conga


instrument2Name : Percussion -> String
instrument2Name i =
    case i of
        Electronic ->
            "Electronic"

        Blocks ->
            "Blocks"

        Kit ->
            "Kit"

        Conga ->
            "Conga"


cycleInstrument2 : Percussion -> Percussion
cycleInstrument2 i =
    case i of
        Electronic ->
            Blocks

        Blocks ->
            Kit

        Kit ->
            Conga

        Conga ->
            Electronic


type alias Settings =
    { bars : Int
    , beatsPerBar : Int
    , beatSplits : Int
    , scale : MusicScale
    , startsOn : StartNote
    , octaveRange : Int
    }


minBars =
    1


maxBars =
    16


minBeatsPerBar =
    2


maxBeatsPerBar =
    7


minBeatSplits =
    1


maxBeatSplits =
    4


minOctaveRange =
    1


maxOctaveRange =
    3


computeGridWidth : Settings -> Int
computeGridWidth s =
    totalSteps s


totalBeatSplits : Settings -> Int
totalBeatSplits =
    totalSteps


totalSteps : Settings -> Int
totalSteps s =
    s.bars * s.beatsPerBar * s.beatSplits


computeGridHeight : Settings -> Int
computeGridHeight s =
    instrumentGridHeight s + 2


instrumentGridHeight : Settings -> Int
instrumentGridHeight s =
    musicScaleLength s.scale * s.octaveRange


percussionGridHeight : Int
percussionGridHeight =
    2


musicScaleLength : MusicScale -> Int
musicScaleLength musicScale =
    case musicScale of
        Major ->
            7


type StartNote
    = StartNote


type MusicScale
    = Major


initialSettings : Settings
initialSettings =
    { bars = 4
    , beatsPerBar = 4
    , beatSplits = 2
    , scale = Major
    , startsOn = StartNote
    , octaveRange = 2
    }


type PlayerState
    = Playing Float
    | NotPlaying


type Tool
    = Drawing
    | Erasing


type GridType
    = InstrumentGrid
    | PercussionGrid


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    let
        settings =
            initialSettings

        w =
            computeGridWidth settings

        h =
            computeGridHeight settings
    in
    let
        initialPP : PaintedPositions
        initialPP =
            rangeWH w h
                |> Random.List.shuffle
                |> Random.andThen Random.List.shuffle
                |> stepWithInitialSeed 2
                |> List.take 30
                |> Set.fromList

        paintedPositions =
            url.path
                |> String.dropLeft 1
                |> Url.percentDecode
                |> Maybe.withDefault ""
                |> JD.decodeString paintedPositionsDecoder
                |> Result.withDefault initialPP

        igh =
            instrumentGridHeight settings
    in
    ( { instrumentPositions =
            paintedPositions
                |> Set.filter (second >> (\y -> y < igh))
      , percussionPositions =
            paintedPositions
                |> Set.filter (second >> (\y -> y >= igh))
                |> Set.map (mapSecond (add -igh))
      , stepIndex = 0
      , playState = NotPlaying
      , drawState = Nothing
      , settingsDialog = Nothing
      , settings = settings
      , instrument = Piano
      , percussion = Electronic
      , tempo = 120
      , audioTime = 0
      , key = key
      }
    , Cmd.none
    )


paintedPositionsDecoder : Decoder PaintedPositions
paintedPositionsDecoder =
    JD.map Set.fromList (JD.list (JD.map2 Tuple.pair (JD.index 0 JD.int) (JD.index 1 JD.int)))


paintedPositionsEncoder : PaintedPositions -> Value
paintedPositionsEncoder =
    JE.set (\( a, b ) -> JE.list identity [ JE.int a, JE.int b ])


type alias Note =
    { preset : String
    , atAudioTime : Float
    , pitch : String
    , duration : Float
    }


stepDurationInMilli : Model -> Float
stepDurationInMilli model =
    let
        beatDurationInMilli =
            (60 * 1000) / toFloat model.tempo

        duration =
            beatDurationInMilli / toFloat model.settings.beatSplits
    in
    duration


type alias NotePresetAndPitch =
    ( String, String )


instrumentNoteFromGP : Float -> Model -> Int2 -> Note
instrumentNoteFromGP audioTime model ( _, y ) =
    let
        noteNames =
            [ "C3"
            , "D3"
            , "E3"
            , "F3"
            , "G3"
            , "A3"
            , "B3"
            , "C4"
            , "D4"
            , "E4"
            , "F4"
            , "G4"
            , "A4"
            , "B4"
            ]

        presetName =
            case model.instrument of
                Piano ->
                    "piano"

                Strings ->
                    "strings"

                _ ->
                    "strings"
    in
    { preset = presetName
    , atAudioTime = audioTime
    , pitch = listGetAtOrDefault "" y noteNames
    , duration = stepDurationInMilli model
    }


percussionNoteFromGP : Float -> Model -> Int2 -> Note
percussionNoteFromGP audioTime model ( _, y ) =
    let
        ( presetName, pitch ) =
            if y == 0 then
                case model.percussion of
                    Electronic ->
                        ( "snareDrum2", "40" )

                    Blocks ->
                        ( "snareDrum2", "40" )

                    _ ->
                        ( "snareDrum2", "40" )

            else if y == 1 then
                case model.percussion of
                    Electronic ->
                        ( "bassDrum1", "36" )

                    Blocks ->
                        ( "bassDrum1", "36" )

                    _ ->
                        ( "bassDrum1", "36" )

            else
                Debug.todo (Debug.toString y)
    in
    { preset = presetName
    , atAudioTime = audioTime
    , pitch = pitch
    , duration = stepDurationInMilli model
    }


notePresetAndPitchFromGP : Model -> Int2 -> NotePresetAndPitch
notePresetAndPitchFromGP model ( _, y ) =
    let
        noteNames =
            [ "C3"
            , "D3"
            , "E3"
            , "F3"
            , "G3"
            , "A3"
            , "B3"
            , "C4"
            , "D4"
            , "E4"
            , "F4"
            , "G4"
            , "A4"
            , "B4"
            ]
    in
    if y < 14 then
        case model.instrument of
            Piano ->
                ( "piano", listGetAtOrDefault "" y noteNames )

            Strings ->
                ( "strings", listGetAtOrDefault "" y noteNames )

            _ ->
                ( "strings", listGetAtOrDefault "" y noteNames )

    else if y == 14 then
        case model.percussion of
            Electronic ->
                ( "snareDrum2", "40" )

            Blocks ->
                ( "snareDrum2", "40" )

            _ ->
                ( "snareDrum2", "40" )

    else if y == 15 then
        case model.percussion of
            Electronic ->
                ( "bassDrum1", "36" )

            Blocks ->
                ( "bassDrum1", "36" )

            _ ->
                ( "bassDrum1", "36" )

    else
        Debug.todo (Debug.toString y)


noteColorFromGP : Int2 -> String
noteColorFromGP ( _, y ) =
    let
        colors =
            [ wPink
            , wPurple
            , wBlue
            , wGreen2_sea
            , wOrange
            , wYellow
            , wGreen_lime
            ]
    in
    listGetAtOrDefault "" (modBy 7 y) colors


type Msg
    = NOP
    | PointerDownOnGP GridType Int2
    | PointerEnteredGP GridType Int2
    | OnPointerUp
    | OnBrowserKeyDown KeyEvent
    | OnAudioContextTime Float
      -- Bottom Bar
    | TogglePlayClicked
    | InstrumentButtonClicked
    | PercussionButtonClicked
    | TempoInputChanged String
    | SettingsClicked
      -- Settings Dialog
    | CloseSettingsClicked
    | SaveSettingsClicked
    | BarCountChanged String
    | BeatsPerBarChanged String
    | BeatSplitsChanged String
    | OctaveRangeChanged String


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ onBrowserKeyDown OnBrowserKeyDown
    , onAudioContextTime OnAudioContextTime
    ]
        |> Sub.batch


playInstrumentNoteAtGPCmd : Model -> Int2 -> Cmd msg
playInstrumentNoteAtGPCmd model gp =
    scheduleNote (instrumentNoteFromGP model.audioTime model gp)


playPercussionNoteAtGPCmd : Model -> Int2 -> Cmd msg
playPercussionNoteAtGPCmd model gp =
    scheduleNote (percussionNoteFromGP model.audioTime model gp)


focusOrIgnoreCmd : String -> Cmd Msg
focusOrIgnoreCmd id =
    Browser.Dom.focus id
        |> Task.attempt (always NOP)


scheduleCurrentStepAtEffect : Float -> Model -> Cmd Msg
scheduleCurrentStepAtEffect atAudioTime model =
    [ model.instrumentPositions
        |> Set.filter (first >> eq model.stepIndex)
        |> Set.toList
        |> List.map (instrumentNoteFromGP atAudioTime model >> scheduleNote)
        |> Cmd.batch
    , model.percussionPositions
        |> Set.filter (first >> eq model.stepIndex)
        |> Set.toList
        |> List.map (percussionNoteFromGP atAudioTime model >> scheduleNote)
        |> Cmd.batch
    ]
        |> Cmd.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        PointerDownOnGP gt gp ->
            case gt of
                InstrumentGrid ->
                    if Set.member gp model.instrumentPositions then
                        { model
                            | instrumentPositions = Set.remove gp model.instrumentPositions
                            , drawState = Just ( Erasing, InstrumentGrid )
                        }
                            |> withNoCmd

                    else
                        { model
                            | instrumentPositions = Set.insert gp model.instrumentPositions
                            , drawState = Just ( Drawing, InstrumentGrid )
                        }
                            |> withCmd (playInstrumentNoteAtGPCmd model gp)

                PercussionGrid ->
                    if Set.member gp model.percussionPositions then
                        { model
                            | percussionPositions = Set.remove gp model.percussionPositions
                            , drawState = Just ( Erasing, PercussionGrid )
                        }
                            |> withNoCmd

                    else
                        { model
                            | percussionPositions = Set.insert gp model.percussionPositions
                            , drawState = Just ( Drawing, PercussionGrid )
                        }
                            |> withCmd (playPercussionNoteAtGPCmd model gp)

        PointerEnteredGP gt gp ->
            case model.drawState of
                Nothing ->
                    ( model, Cmd.none )

                Just ( tool, activeGT ) ->
                    if activeGT == gt then
                        case ( tool, gt ) of
                            ( Drawing, InstrumentGrid ) ->
                                { model | instrumentPositions = Set.insert gp model.instrumentPositions }
                                    |> withCmd (playInstrumentNoteAtGPCmd model gp)

                            ( Erasing, InstrumentGrid ) ->
                                { model | instrumentPositions = Set.remove gp model.instrumentPositions }
                                    |> withNoCmd

                            ( Drawing, PercussionGrid ) ->
                                { model | percussionPositions = Set.insert gp model.percussionPositions }
                                    |> withCmd (playPercussionNoteAtGPCmd model gp)

                            ( Erasing, PercussionGrid ) ->
                                { model | percussionPositions = Set.remove gp model.percussionPositions }
                                    |> withNoCmd

                    else
                        ( model, Cmd.none )

        OnPointerUp ->
            ( { model | drawState = Nothing }, Cmd.none )

        OnAudioContextTime currentAudioTime ->
            updateAfterAudioTimeReceived { model | audioTime = currentAudioTime }

        OnBrowserKeyDown e ->
            if e.isTargetBodyElement && not e.repeat && e.key == " " then
                updateOnTogglePlay model

            else if e.key == "s" then
                ( model
                , --Browser.Navigation.replaceUrl model.key
                  --    (paintedPositionsEncoder model.paintedPositions |> JE.encode 0)
                  Cmd.none
                )

            else
                ( model, Cmd.none )

        TogglePlayClicked ->
            updateOnTogglePlay model

        InstrumentButtonClicked ->
            ( { model | instrument = cycleInstrument1 model.instrument }
            , Cmd.none
            )

        PercussionButtonClicked ->
            ( { model | percussion = cycleInstrument2 model.percussion }
            , Cmd.none
            )

        TempoInputChanged str ->
            let
                tempo =
                    String.toInt str
                        |> Maybe.withDefault model.tempo
                        |> clamp 10 300
            in
            ( { model | tempo = tempo }, Cmd.none )

        SettingsClicked ->
            ( { model | settingsDialog = Just model.settings }, focusOrIgnoreCmd "cancel-settings-btn" )

        CloseSettingsClicked ->
            ( { model | settingsDialog = Nothing }
            , focusOrIgnoreCmd "settings-btn"
            )

        SaveSettingsClicked ->
            case model.settingsDialog of
                Nothing ->
                    ( model, Cmd.none )

                Just newSettings ->
                    ( { model
                        | settingsDialog = Nothing

                        --, settings = newSettings
                        --, paintedPositions =
                        --    resizePaintedPositions
                        --        model.settings
                        --        newSettings
                        --        model.paintedPositions
                      }
                    , focusOrIgnoreCmd "settings-btn"
                    )

        BarCountChanged str ->
            updateSettingsForm
                (\s ->
                    { s
                        | bars =
                            String.toInt str
                                |> Maybe.map (clamp minBars maxBars)
                                |> Maybe.withDefault s.bars
                    }
                )
                model

        BeatsPerBarChanged str ->
            updateSettingsForm
                (\s ->
                    { s
                        | beatsPerBar =
                            String.toInt str
                                |> Maybe.map (clamp minBeatsPerBar maxBeatsPerBar)
                                |> Maybe.withDefault s.beatsPerBar
                    }
                )
                model

        BeatSplitsChanged str ->
            updateSettingsForm
                (\s ->
                    { s
                        | beatSplits =
                            String.toInt str
                                |> Maybe.map (clamp minBeatSplits maxBeatSplits)
                                |> Maybe.withDefault s.beatSplits
                    }
                )
                model

        OctaveRangeChanged str ->
            updateSettingsForm
                (\s ->
                    { s
                        | octaveRange =
                            String.toInt str
                                |> Maybe.map (clamp minOctaveRange maxOctaveRange)
                                |> Maybe.withDefault s.octaveRange
                    }
                )
                model


updateSettingsForm : (Settings -> Settings) -> Model -> ( Model, Cmd msg )
updateSettingsForm fn model =
    mapSettingsForm fn model |> withNoCmd


mapSettingsForm : (Settings -> Settings) -> Model -> Model
mapSettingsForm fn model =
    case model.settingsDialog of
        Just s ->
            { model | settingsDialog = Just (fn s) }

        Nothing ->
            model


updateOnTogglePlay : Model -> ( Model, Cmd Msg )
updateOnTogglePlay model =
    case model.playState of
        NotPlaying ->
            let
                initialDelay =
                    100

                currentStepAudioTime =
                    model.audioTime + initialDelay

                nextStepAudioTime =
                    currentStepAudioTime + stepDurationInMilli model
            in
            { model
                | playState = Playing nextStepAudioTime
                , stepIndex = 0
            }
                |> withEffect (scheduleCurrentStepAtEffect currentStepAudioTime)

        Playing _ ->
            { model | playState = NotPlaying } |> withNoCmd


updateAfterAudioTimeReceived : Model -> ( Model, Cmd Msg )
updateAfterAudioTimeReceived model =
    case model.playState of
        NotPlaying ->
            ( model, Cmd.none )

        Playing nextStepAudioTime_ ->
            let
                diff =
                    nextStepAudioTime_ - model.audioTime |> atLeast 0
            in
            if diff < 100 then
                let
                    currentStepAudioTime =
                        model.audioTime + diff

                    nextStepAudioTime =
                        currentStepAudioTime + stepDurationInMilli model
                in
                { model
                    | stepIndex = model.stepIndex + 1 |> modBy (totalSteps model.settings)
                    , playState = Playing nextStepAudioTime
                }
                    |> withEffect (scheduleCurrentStepAtEffect currentStepAudioTime)

            else
                ( model, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "Song Maker"
        [ basicStylesNode
        , animateCssNode
        , case model.settingsDialog of
            Nothing ->
                view model

            Just s ->
                viewSettingsForm s
        ]


type alias LCR a =
    ( List a, a, List a )


lcrToList : LCR a -> List a
lcrToList ( l, c, r ) =
    l ++ c :: r


lcrMap : (a -> b) -> LCR a -> LCR b
lcrMap fn ( l, c, r ) =
    ( List.map fn l, fn c, List.map fn r )


lcrMapCS : (a -> b) -> (a -> b) -> LCR a -> LCR b
lcrMapCS fc fs ( l, c, r ) =
    ( List.map fs l, fc c, List.map fs r )


lcrRange : Int -> Int -> Int -> LCR Int
lcrRange lo c hi =
    ( List.range lo (c - 1)
    , c
    , List.range (c + 1) hi
    )


viewSettingsForm : Settings -> Html Msg
viewSettingsForm s =
    let
        barsOptions =
            lcrRange minBars s.bars maxBars
                |> lcrMap fromInt
    in
    fCol [ pa "20px", gap "20px" ]
        [ div [ fontSize "22px" ] [ text "SETTINGS" ]
        , Html.label []
            [ text "Length (in Bars): "
            , viewSelectLCR BarCountChanged barsOptions
            ]
        , Html.label []
            [ text "Beats per bar: "
            , viewSelectLCR BeatsPerBarChanged
                (lcrRange minBeatsPerBar s.beatsPerBar maxBeatsPerBar
                    |> lcrMap fromInt
                )
            ]
        , Html.label []
            [ text "Split beats into: "
            , viewSelectLCR BeatSplitsChanged
                (lcrRange minBeatSplits s.beatSplits maxBeatSplits
                    |> lcrMap fromInt
                )
            ]
        , Html.label [] [ text "Scale: ", viewSelect [ "Major", "Minor", "Chromatic" ] ]
        , Html.label []
            [ text "Start on: "
            , viewSelect [ "Middle", "Low", "High" ]
            , viewSelect [ "C", "C#", "B" ]
            ]
        , Html.label []
            [ text "Range (in Octave): "
            , viewSelectLCR OctaveRangeChanged
                (lcrRange minOctaveRange s.octaveRange maxOctaveRange
                    |> lcrMap fromInt
                )
            ]
        , fRow [ gap "20px" ]
            [ viewBtn [ notifyClick SaveSettingsClicked ] "Ok"
            , viewBtn
                [ HA.id "cancel-settings-btn"
                , notifyClick CloseSettingsClicked
                ]
                "Cancel"
            ]
        ]


viewBtn aa s =
    button
        ([ fontSize "20px"
         , pa "0.5ch 1ch"
         ]
            ++ aa
        )
        [ text s ]


viewSelect l =
    Html.select [ fontSize "20px" ] (l |> List.map (\s -> Html.option [] [ text s ]))


viewSelectLCR msg lcr =
    Html.select [ fontSize "20px", onInput msg ]
        (lcrMapCS
            (\c -> Html.option [ HA.selected True ] [ text c ])
            (\s -> Html.option [] [ text s ])
            lcr
            |> lcrToList
        )


view : Model -> Html Msg
view model =
    fCol []
        [ viewGrid model
        , viewBottomBar model
        ]


viewBottomBar : Model -> Html Msg
viewBottomBar model =
    fRow
        [ pa "20px"
        , gap "20px"
        , itemsCenter
        ]
        [ viewPlayButton model.playState
        , viewBtn
            [ sWidth "14ch"
            , notifyClick InstrumentButtonClicked
            ]
            (instrument1Name model.instrument)
        , viewBtn
            [ sWidth "14ch"
            , notifyClick PercussionButtonClicked
            ]
            (instrument2Name model.percussion)
        , viewTempoInput model.tempo
        , viewSettingsButton
        , viewBtn [] "Undo"
        , viewBtn [] "Save"
        ]


viewSettingsButton =
    viewBtn
        [ HA.id "settings-btn", notifyClick SettingsClicked ]
        "Settings"


viewTempoInput tempo =
    Html.label []
        [ text "Tempo "
        , Html.input
            [ HA.value (fromInt tempo)
            , onInput TempoInputChanged
            , HA.size 4
            , HA.type_ "number"
            , fontSize "20px"
            , sWidth "5ch"
            ]
            []
        ]


viewPlayButton : PlayerState -> Html Msg
viewPlayButton playState =
    button
        [ autofocus True
        , fontSize "20px"
        , pa "0.5ch 1ch"
        , notifyClick TogglePlayClicked
        ]
        [ span [ style "display" "inline-block", sMinWidth "4ch" ]
            [ text
                (case playState of
                    Playing _ ->
                        "Stop"

                    NotPlaying ->
                        "Play"
                )
            ]
        ]


viewGrid : Model -> Html Msg
viewGrid model =
    fCol
        [ positionRelative
        , style "flex-grow" "1"
        , notifyPointerUp OnPointerUp
        , noUserSelect
        ]
        [ div [ dGrid, positionRelative, style "flex-grow" "1" ]
            [ let
                w =
                    computeGridWidth model.settings

                h =
                    instrumentGridHeight model.settings
              in
              div [ dGrid, styleGridTemplate w h ]
                (rangeWH w h |> List.map (viewInstrumentTileAt model))
            , viewInstrumentGridLines model.settings
            ]
        , div [ dGrid, positionRelative, sHeight "20%" ]
            [ let
                w =
                    computeGridWidth model.settings

                h =
                    percussionGridHeight
              in
              div [ dGrid, styleGridTemplate w h ]
                (rangeWH w h |> List.map (viewPercussionTileAt model))
            , viewPercussionGridLines model.settings
            ]
        ]


viewInstrumentGridLines : Settings -> Html msg
viewInstrumentGridLines s =
    let
        ( w, h ) =
            ( toFloat (computeGridWidth s), toFloat (instrumentGridHeight s) )
    in
    div
        [ w100
        , h100
        , positionAbsolute
        , noPointerEvents
        , backgrounds
            (List.reverse
                [ -- minor grid lines
                  backgroundGridLinesVertical 1 (grayN 0.16) (1 / w)
                , backgroundGridLinesHorizontal 1 (grayN 0.16) (1 / h)

                -- major grid lines
                , backgroundGridLinesVertical 2 (grayN 0.3) (toFloat s.beatSplits / w)
                , backgroundGridLinesHorizontal 3
                    (grayN 0.3)
                    (toFloat (musicScaleLength s.scale) / h)
                ]
            )
        ]
        []


viewPercussionGridLines : Settings -> Html msg
viewPercussionGridLines s =
    let
        w =
            toFloat (computeGridWidth s)
    in
    div
        [ w100
        , h100
        , positionAbsolute
        , noPointerEvents
        , backgrounds
            (List.reverse
                [ -- minor grid lines
                  backgroundGridLinesVertical 1 (grayN 0.16) (1 / w)

                --, backgroundGridLinesHorizontal 1 (grayN 0.16) (1 / h)
                -- major grid lines
                , backgroundGridLinesVertical 2 (grayN 0.3) (toFloat s.beatSplits / w)

                --, backgroundGridLinesHorizontal 3
                --    (grayN 0.3)
                --    (toFloat (musicScaleLength s.scale) / h)
                ]
            )
        ]
        []


viewGridLines : Settings -> Html msg
viewGridLines s =
    let
        ( w, h ) =
            ( toFloat (computeGridWidth s), toFloat (computeGridHeight s) )
    in
    div
        [ w100
        , h100
        , positionAbsolute
        , noPointerEvents
        , backgrounds
            (List.reverse
                [ -- minor grid lines
                  backgroundGridLinesVertical 1 (grayN 0.16) (1 / w)
                , backgroundGridLinesHorizontal 1 (grayN 0.16) (1 / h)

                -- major grid lines
                , backgroundGridLinesVertical 2 (grayN 0.3) (toFloat s.beatSplits / w)
                , backgroundGridLinesHorizontal 3 (grayN 0.3) (7 / h)
                ]
            )
        ]
        []


type alias BeatSplit =
    List Int


type alias Beat =
    ( ( BeatSplit, BeatSplit ), ( BeatSplit, BeatSplit ) )


emptyBeat : Beat
emptyBeat =
    ( ( [], [] ), ( [], [] ) )


type alias Bar =
    List Beat


resizePaintedPositions : Settings -> Settings -> PaintedPositions -> PaintedPositions
resizePaintedPositions from to =
    paintedPositionsToBars from
        >> resizeBarsToPaintedPositions to


paintedPositionsToBars : Settings -> PaintedPositions -> List Bar
paintedPositionsToBars settings pp =
    let
        ll : List Int2
        ll =
            Set.toList pp

        beatSplitAtX : Int -> BeatSplit
        beatSplitAtX x =
            List.filter (first >> eq x) ll |> List.map second

        beatSplits : List BeatSplit
        beatSplits =
            List.range 0 (totalBeatSplits settings)
                |> List.map beatSplitAtX

        beatFromBeatSplits : List BeatSplit -> Beat
        beatFromBeatSplits splits =
            case splits of
                first :: second :: [] ->
                    ( ( first, [] ), ( second, [] ) )

                first :: second :: third :: [] ->
                    ( ( first, second ), ( third, [] ) )

                first :: second :: third :: fourth :: [] ->
                    ( ( first, second ), ( third, fourth ) )

                _ ->
                    emptyBeat

        beats : List Beat
        beats =
            List.Extra.groupsOf settings.beatSplits beatSplits
                |> List.map beatFromBeatSplits

        bars : List Bar
        bars =
            List.Extra.groupsOf settings.bars beats
    in
    bars


resizeBarsToPaintedPositions : Settings -> List Bar -> PaintedPositions
resizeBarsToPaintedPositions settings bars =
    let
        beatToBeatSplits : Beat -> List BeatSplit
        beatToBeatSplits ( ( first, second ), ( third, fourth ) ) =
            case settings.beatSplits of
                2 ->
                    [ first, third ]

                3 ->
                    [ first, second, third ]

                4 ->
                    [ first, second, third, fourth ]

                _ ->
                    []

        barsToPaintedPositions : List Bar -> PaintedPositions
        barsToPaintedPositions =
            List.concat
                >> List.concatMap beatToBeatSplits
                >> List.indexedMap (\x -> List.map (pair x))
                >> List.concat
                >> Set.fromList
    in
    List.map (listResize emptyBeat settings.beatsPerBar) bars
        |> listResize [] settings.bars
        |> barsToPaintedPositions


listResize : a -> Int -> List a -> List a
listResize default toLength list =
    let
        fromLength =
            List.length list
    in
    case compare fromLength toLength of
        LT ->
            list ++ List.repeat (toLength - fromLength) default

        EQ ->
            list

        GT ->
            List.take toLength list


styleGridTemplate : Int -> Int -> Attribute msg
styleGridTemplate w h =
    style "grid-template"
        (("repeat(" ++ fromInt h ++ ",1fr)")
            ++ "/"
            ++ ("repeat(" ++ fromInt w ++ ",1fr)")
        )


highlightBGColor =
    hsl 0.8 1 0.2


barBGColor2 =
    hsl 0 0 0.2


backgrounds =
    style "background" << String.join ","


backgroundGridLinesVertical strokeWidth color pctN =
    [ "linear-gradient(to right, " ++ color ++ fromFloat strokeWidth ++ "px, transparent 0px)"
    , fromFloat (strokeWidth / -2) ++ "px"
    , "0"
    , "/"
    , fromFloat (pctN * 100) ++ "%"
    , "100%"
    ]
        |> String.join " "


backgroundGridLinesHorizontal strokeWidth color pctN =
    [ "linear-gradient(to bottom, " ++ color ++ fromFloat strokeWidth ++ "px, transparent 0px)"
    , "0"
    , fromFloat (strokeWidth / -2) ++ "px"
    , "/"
    , "100%"
    , fromFloat (pctN * 100) ++ "%"
    ]
        |> String.join " "


viewPercussionTileAt : Model -> Int2 -> Html Msg
viewPercussionTileAt model (( x, _ ) as gp) =
    let
        isPlaying =
            case model.playState of
                Playing _ ->
                    True

                _ ->
                    False

        isNoteTile =
            Set.member gp model.percussionPositions

        isHighlightedTile =
            x == model.stepIndex

        anim =
            if isPlaying && isNoteTile && isHighlightedTile then
                blink

            else
                Animation.empty

        notesPerBar =
            model.settings.beatsPerBar * model.settings.beatSplits

        isAlternateBarTile =
            modBy (notesPerBar * 2) x >= notesPerBar

        bgColor =
            if isNoteTile then
                noteColorFromGP gp

            else if isHighlightedTile then
                highlightBGColor

            else if isAlternateBarTile then
                barBGColor2

            else
                "transparent"
    in
    Animated.div
        anim
        [ bgc bgColor
        , styleGridAreaFromGP gp
        , notifyPointerDown (PointerDownOnGP PercussionGrid gp)
        , notifyPointerEnter (PointerEnteredGP PercussionGrid gp)
        ]
        []


viewInstrumentTileAt : Model -> Int2 -> Html Msg
viewInstrumentTileAt model (( x, _ ) as gp) =
    let
        isPlaying =
            case model.playState of
                Playing _ ->
                    True

                _ ->
                    False

        isNoteTile =
            Set.member gp model.instrumentPositions

        isHighlightedTile =
            x == model.stepIndex

        anim =
            if isPlaying && isNoteTile && isHighlightedTile then
                blink

            else
                Animation.empty

        notesPerBar =
            model.settings.beatsPerBar * model.settings.beatSplits

        isAlternateBarTile =
            modBy (notesPerBar * 2) x >= notesPerBar

        bgColor =
            if isNoteTile then
                noteColorFromGP gp

            else if isHighlightedTile then
                highlightBGColor

            else if isAlternateBarTile then
                barBGColor2

            else
                "transparent"
    in
    Animated.div
        anim
        [ bgc bgColor
        , styleGridAreaFromGP gp
        , notifyPointerDown (PointerDownOnGP InstrumentGrid gp)
        , notifyPointerEnter (PointerEnteredGP InstrumentGrid gp)
        ]
        []


styleGridAreaFromGP : Int2 -> Attribute msg
styleGridAreaFromGP ( x, y ) =
    let
        ( row, col ) =
            ( y + 1, x + 1 )
    in
    style "grid-area" (fromInt row ++ "/" ++ fromInt col)


blink : Animation
blink =
    Animation.steps
        { startAt = [ P.opacity 1 ]
        , options = []
        }
        [ Animation.step 50 [ P.opacity 0.2 ]
        , let
            barLengthSec =
                2

            noteGapSec =
                (1 / 8) * barLengthSec

            noteGapMilli =
                noteGapSec * 1000 |> round
          in
          Animation.step noteGapMilli [ P.opacity 1 ]
        ]
