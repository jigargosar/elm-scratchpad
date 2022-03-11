port module SongMakerSF exposing (main)

import Browser.Dom
import Browser.Navigation exposing (Key)
import Html
import Html.Attributes as HA
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra
import Pivot exposing (Pivot)
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
    * start noteName
    * Undo
    * Save




-}


port scheduleNote : Note -> Cmd msg


scheduleNotes : List Note -> Cmd msg
scheduleNotes =
    List.map scheduleNote >> Cmd.batch


port onAudioContextTime : (Float -> msg) -> Sub msg


main =
    browserApplication
        { init = init
        , onUrlRequest = Debug.log "onUrlRequest: " >> always NOP
        , onUrlChange = Debug.log "onUrlChange: " >> UrlChanged
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias PaintedPositions =
    Set Int2


type alias Model =
    { instrumentPositions : PaintedPositions
    , percussionPositions : PaintedPositions
    , settings : Settings
    , instrument : Instrument
    , percussion : Percussion
    , tempo : Int
    , stepIndex : Int
    , playState : PlayerState
    , drawState : Maybe ( Tool, GridType )
    , settingsDialog : Maybe Settings
    , audioTime : Float
    , key : Key
    , url : Url
    }


applyDataModel : DataModel -> Model -> Model
applyDataModel dataModel model =
    { model
        | instrumentPositions = dataModel.instrumentPositions
        , percussionPositions = dataModel.percussionPositions
        , settings = dataModel.settings
        , instrument = dataModel.instrument
        , percussion = dataModel.percussion
        , tempo = dataModel.tempo
    }


toDataModel : Model -> DataModel
toDataModel model =
    { instrumentPositions = model.instrumentPositions
    , percussionPositions = model.percussionPositions
    , settings = model.settings
    , instrument = model.instrument
    , percussion = model.percussion
    , tempo = model.tempo
    }


type alias DataModel =
    { instrumentPositions : PaintedPositions
    , percussionPositions : PaintedPositions
    , settings : Settings
    , instrument : Instrument
    , percussion : Percussion
    , tempo : Int
    }


initialDataModel : DataModel
initialDataModel =
    let
        settings =
            initialSettingsV1

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
    in
    dataModelFromPaintedPositionsV1 initialPP


dataModelFromUrl : Url -> DataModel
dataModelFromUrl url =
    decodeDataModelFromUrl url
        |> Result.withDefault initialDataModel


decodeDataModelFromUrl : Url -> Result JD.Error DataModel
decodeDataModelFromUrl url =
    let
        dataModelDecoder =
            JD.oneOf [ dataModelDecoderV2, dataModelDecoderV1 ]
    in
    payloadStringFromUrl url
        |> JD.decodeString dataModelDecoder


payloadStringFromUrl : Url -> String
payloadStringFromUrl url =
    url.path
        |> String.dropLeft 1
        |> Url.percentDecode
        |> Maybe.withDefault ""


dataModelEncoderV2 : DataModel -> Value
dataModelEncoderV2 dataModel =
    JE.object <|
        [ ( "instrumentPositions", paintedPositionsEncoder dataModel.instrumentPositions )
        , ( "percussionPositions", paintedPositionsEncoder dataModel.percussionPositions )
        , ( "settings", encodeSettings dataModel.settings )
        , ( "instrument", encodeInstrument dataModel.instrument )
        , ( "percussion", encodePercussion dataModel.percussion )
        , ( "tempo", JE.int dataModel.tempo )
        ]


encodeSettings : Settings -> Value
encodeSettings settings =
    JE.object <|
        [ ( "bars", JE.int settings.bars )
        , ( "beatsPerBar", JE.int settings.beatsPerBar )
        , ( "beatSplits", JE.int settings.beatSplits )

        --, ( "scale", encodeMusicScale settings.scale )
        --, ( "startsOn", encodeStartNote settings.startsOn )
        --, ( "octaveRange", JE.int settings.octaveRange )
        ]



--encodeMusicScale : MusicScale -> Value
--encodeMusicScale musicScale =
--    case musicScale of
--        Major ->
--            JE.string "Major"
--
--
--encodeStartNote : StartNote -> Value
--encodeStartNote startNote =
--    case startNote of
--        StartNote ->
--            JE.string "StartNote"
--


encodeInstrument : Instrument -> Value
encodeInstrument instrument =
    case instrument of
        Piano ->
            JE.string "Piano"

        Strings ->
            JE.string "Strings"

        Woodwind ->
            JE.string "Woodwind"

        Synth ->
            JE.string "Synth"

        Marimba ->
            JE.string "Marimba"


encodePercussion : Percussion -> Value
encodePercussion percussion =
    case percussion of
        Electronic ->
            JE.string "Electronic"

        Blocks ->
            JE.string "Blocks"

        Kit ->
            JE.string "Kit"

        Conga ->
            JE.string "Conga"


dataModelDecoderV1 : Decoder DataModel
dataModelDecoderV1 =
    paintedPositionsDecoder |> JD.map dataModelFromPaintedPositionsV1


dataModelFromPaintedPositionsV1 : PaintedPositions -> DataModel
dataModelFromPaintedPositionsV1 paintedPositions =
    let
        settings =
            initialSettingsV1

        igh =
            instrumentGridHeight settings
    in
    { instrumentPositions =
        paintedPositions
            |> Set.filter (second >> (\y -> y < igh))
    , percussionPositions =
        paintedPositions
            |> Set.filter (second >> (\y -> y >= igh))
            |> Set.map (mapSecond (add -igh))
    , settings = settings
    , instrument = Piano
    , percussion = Electronic
    , tempo = 120
    }


dataModelDecoderV2 : Decoder DataModel
dataModelDecoderV2 =
    JD.succeed DataModel
        |> jpRequired "instrumentPositions" paintedPositionsDecoder
        |> jpRequired "percussionPositions" paintedPositionsDecoder
        |> jpRequired "settings" settingsDecoder
        |> jpRequired "instrument" instrumentDecoder
        |> jpRequired "percussion" percussionDecoder
        |> jpRequired "tempo" JD.int


settingsDecoder : Decoder Settings
settingsDecoder =
    JD.succeed Settings
        |> jpRequired "bars" JD.int
        |> jpRequired "beatsPerBar" JD.int
        |> jpRequired "beatSplits" JD.int
        |> jpHardcoded Major
        |> jpHardcoded Mid
        |> jpHardcoded 2


instrumentDecoder : Decoder Instrument
instrumentDecoder =
    let
        get id =
            case id of
                "Piano" ->
                    JD.succeed Piano

                "Strings" ->
                    JD.succeed Strings

                "Woodwind" ->
                    JD.succeed Woodwind

                "Synth" ->
                    JD.succeed Synth

                "Marimba" ->
                    JD.succeed Marimba

                _ ->
                    JD.fail ("unknown value for Instrument: " ++ id)
    in
    JD.string |> JD.andThen get


percussionDecoder : Decoder Percussion
percussionDecoder =
    let
        get id =
            case id of
                "Electronic" ->
                    JD.succeed Electronic

                "Blocks" ->
                    JD.succeed Blocks

                "Kit" ->
                    JD.succeed Kit

                "Conga" ->
                    JD.succeed Conga

                _ ->
                    JD.fail ("unknown value for Percussion: " ++ id)
    in
    JD.string |> JD.andThen get


type Instrument
    = Piano
    | Strings
    | Woodwind
    | Synth
    | Marimba


instrumentName : Instrument -> String
instrumentName i =
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


cycleInstrument : Instrument -> Instrument
cycleInstrument i =
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


percussionName : Percussion -> String
percussionName i =
    case i of
        Electronic ->
            "Electronic"

        Blocks ->
            "Blocks"

        Kit ->
            "Kit"

        Conga ->
            "Conga"


cyclePercussion : Percussion -> Percussion
cyclePercussion i =
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
    , startOctave : Octave
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


type Octave
    = Low
    | Mid
    | High


octaveFromString : String -> Maybe Octave
octaveFromString string =
    case string of
        "Low" ->
            Just Low

        "Mid" ->
            Just Mid

        "High" ->
            Just High

        _ ->
            Nothing


octaveToString : Octave -> String
octaveToString octave =
    case octave of
        Low ->
            "Low"

        Mid ->
            "Mid"

        High ->
            "High"


octaveToInt : Octave -> Int
octaveToInt octave =
    case octave of
        Low ->
            3

        Mid ->
            4

        High ->
            5


startOctaveNum : Octave -> Int -> Int
startOctaveNum startOctave octaveRange =
    case octaveRange of
        1 ->
            octaveToInt startOctave

        _ ->
            octaveToInt startOctave - 1


type MusicScale
    = Major


musicScaleLength : MusicScale -> Int
musicScaleLength =
    noteNamesFromScale >> List.length


noteClassesFromScale : MusicScale -> List Int
noteClassesFromScale musicScale =
    case musicScale of
        Major ->
            List.foldl
                (\ti ( prev, acc ) ->
                    let
                        curr =
                            prev + toneIntervalToHalfTones ti
                    in
                    ( curr, acc ++ [ curr ] )
                )
                ( 0, [ 0 ] )
                majorScaleToneIntervals
                |> second



--[ 0, 2, 4, 5, 7, 9, 11 ]


type ToneInterval
    = HalfTone
    | FullTone


toneIntervalToHalfTones : ToneInterval -> number
toneIntervalToHalfTones ti =
    case ti of
        HalfTone ->
            1

        FullTone ->
            2


majorScaleToneIntervals =
    [ FullTone, FullTone, HalfTone, FullTone, FullTone, FullTone ]


noteNamesFromScale : MusicScale -> List String
noteNamesFromScale s =
    case s of
        Major ->
            [ "C"
            , "D"
            , "E"
            , "F"
            , "G"
            , "A"
            , "B"
            ]


initialSettingsV1 : Settings
initialSettingsV1 =
    { bars = 4
    , beatsPerBar = 4
    , beatSplits = 2
    , scale = Major
    , startOctave = Mid
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
        dataModel =
            dataModelFromUrl url
    in
    ( { instrumentPositions = dataModel.instrumentPositions
      , percussionPositions = dataModel.percussionPositions
      , settings = dataModel.settings
      , instrument = dataModel.instrument
      , percussion = dataModel.percussion
      , tempo = dataModel.tempo
      , stepIndex = 0
      , playState = NotPlaying
      , drawState = Nothing
      , settingsDialog = Nothing
      , audioTime = 0
      , key = key
      , url = url
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


scheduleInstrumentNotes : Float -> Model -> List Int -> Cmd msg
scheduleInstrumentNotes audioTime model ys =
    instrumentPitchesFromYS model.settings ys
        |> List.map (instrumentNoteFromPitch audioTime model)
        |> scheduleNotes


instrumentPitchesFromYS : Settings -> List Int -> List String
instrumentPitchesFromYS settings ys =
    let
        pitches =
            instrumentPitches settings

        pitchAt y =
            case listGetAt y pitches of
                Nothing ->
                    Debug.todo "instrumentPitchesFromYS: invalid y"

                Just a ->
                    a
    in
    List.map pitchAt ys


instrumentPitches : Settings -> List String
instrumentPitches settings =
    let
        octaveStart =
            startOctaveNum settings.startOctave settings.octaveRange

        noteNames =
            noteClassesFromScale settings.scale
    in
    List.range octaveStart (octaveStart + settings.octaveRange - 1)
        |> List.map (\i -> List.map (\n -> (12 * (i + 1)) + n |> fromInt) noteNames)
        |> List.concat


instrumentNoteFromPitch : Float -> Model -> String -> Note
instrumentNoteFromPitch audioTime model pitch =
    let
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
    , pitch = pitch
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
    | UrlChanged Url
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
    | StartOctaveChanged String
    | OctaveRangeChanged String


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ onBrowserKeyDown OnBrowserKeyDown
    , onAudioContextTime OnAudioContextTime
    ]
        |> Sub.batch


playInstrumentNoteAtGPCmd : Model -> Int2 -> Cmd msg
playInstrumentNoteAtGPCmd model ( _, y ) =
    scheduleInstrumentNotes model.audioTime model [ y ]


playPercussionNoteAtGPCmd : Model -> Int2 -> Cmd msg
playPercussionNoteAtGPCmd model gp =
    scheduleNote (percussionNoteFromGP model.audioTime model gp)


focusOrIgnoreCmd : String -> Cmd Msg
focusOrIgnoreCmd id =
    Browser.Dom.focus id
        |> Task.attempt (always NOP)


scheduleCurrentStepAtEffect : Float -> Model -> Cmd Msg
scheduleCurrentStepAtEffect atAudioTime model =
    [ let
        igh =
            instrumentGridHeight model.settings
      in
      model.instrumentPositions
        |> Set.toList
        |> keep (first >> eq model.stepIndex)
        |> reject (second >> (\y -> y >= igh))
        |> List.map second
        |> scheduleInstrumentNotes atAudioTime model
    , model.percussionPositions
        |> Set.filter (first >> eq model.stepIndex)
        |> Set.toList
        |> List.map (percussionNoteFromGP atAudioTime model >> scheduleNote)
        |> Cmd.batch
    ]
        |> Cmd.batch


updateBrowserUrlEffect : Model -> Cmd msg
updateBrowserUrlEffect model =
    let
        new =
            dataModelEncoderV2 (toDataModel model) |> JE.encode 0

        old =
            payloadStringFromUrl model.url
    in
    if new /= old then
        Browser.Navigation.pushUrl model.key new

    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        UrlChanged url ->
            case decodeDataModelFromUrl url of
                Err err ->
                    Debug.todo (JD.errorToString err)

                Ok dataModel ->
                    ( applyDataModel dataModel { model | url = url }, Cmd.none )

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
            if e.isTargetBodyElement && not e.repeat && matchesNoModifiers [ " " ] e then
                updateOnTogglePlay model

            else if not e.repeat && matchesNoModifiers [ "p" ] e then
                updateOnTogglePlay model

            else if e.key == "s" then
                model
                    |> withEffect updateBrowserUrlEffect

            else
                ( model, Cmd.none )

        TogglePlayClicked ->
            updateOnTogglePlay model

        InstrumentButtonClicked ->
            ( { model | instrument = cycleInstrument model.instrument }
            , Cmd.none
            )

        PercussionButtonClicked ->
            ( { model | percussion = cyclePercussion model.percussion }
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
                        , settings = newSettings
                        , instrumentPositions =
                            resizeXPositions
                                model.settings
                                newSettings
                                model.instrumentPositions
                        , percussionPositions =
                            resizeXPositions
                                model.settings
                                newSettings
                                model.percussionPositions
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

        StartOctaveChanged str ->
            updateSettingsForm2
                (\startOctave s -> { s | startOctave = startOctave })
                (octaveFromString str)
                model

        OctaveRangeChanged str ->
            updateSettingsForm2
                (\octaveRange s -> { s | octaveRange = octaveRange })
                (String.toInt str |> Maybe.map (clamp minOctaveRange maxOctaveRange))
                model


updateSettingsForm2 : (a -> Settings -> Settings) -> Maybe a -> Model -> ( Model, Cmd msg )
updateSettingsForm2 fn maybe model =
    case ( model.settingsDialog, maybe ) of
        ( Just s, Just a ) ->
            { model | settingsDialog = Just (fn a s) } |> withNoCmd

        _ ->
            model |> withNoCmd


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


lcrFromPivot : Pivot a -> LCR a
lcrFromPivot p =
    ( Pivot.getL p, Pivot.getC p, Pivot.getR p )


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
        , Html.label [] [ text "Scale: ", viewSelect [ "Major" ] ]
        , Html.label []
            [ text "Start on: "
            , viewSelectLCR StartOctaveChanged
                (Pivot.fromCons Low [ Mid, High ]
                    |> withRollback (Pivot.firstWith (eq s.startOctave))
                    |> Pivot.mapA octaveToString
                    |> lcrFromPivot
                )
            , viewSelect [ "C" ]
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
            (instrumentName model.instrument)
        , viewBtn
            [ sWidth "14ch"
            , notifyClick PercussionButtonClicked
            ]
            (percussionName model.percussion)
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


type alias BeatSplit =
    List Int


type alias Beat =
    ( ( BeatSplit, BeatSplit ), ( BeatSplit, BeatSplit ) )


emptyBeat : Beat
emptyBeat =
    ( ( [], [] ), ( [], [] ) )


type alias Bar =
    List Beat


resizeXPositions : Settings -> Settings -> PaintedPositions -> PaintedPositions
resizeXPositions from to =
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
