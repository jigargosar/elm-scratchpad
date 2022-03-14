port module SongMakerSF exposing (main)

import Browser.Dom
import Browser.Navigation exposing (Key)
import Html
import Html.Attributes as HA
import Html.Lazy
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
    * [x] start noteName
    * [x] chromatic scale
    --
    * Undo
        * reset on navigation
        * introduce undo pivot and explicitly add changes in every message.
        * tempo: throttle / delay applying. for easier undo.
    * percussion grid visuals
    --
    * Save btn
    * pentatonic scale
    * instrument/percussion backend
    * chromatic scale colors
    * additional settings persistence



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
    { dataModelPivot : Pivot DataModel
    , stepIndex : Int
    , playState : PlayerState
    , transientState : TransientState
    , audioTime : Float
    , key : Key
    , url : Url
    }


type TransientState
    = SettingsDialog Settings
    | Drawing Tool GridType
    | None


currentTool : GridType -> TransientState -> Maybe Tool
currentTool gridType ts =
    case ts of
        Drawing tool gridType_ ->
            if gridType == gridType_ then
                Just tool

            else
                Nothing

        _ ->
            Nothing


pushDataModel : DataModel -> Model -> Model
pushDataModel dataModel model =
    if dataModel /= currentDataModel model then
        { model | dataModelPivot = Pivot.appendGoR dataModel model.dataModelPivot }

    else
        model


mapPushDataModel : (DataModel -> DataModel) -> Model -> Model
mapPushDataModel fn model =
    pushDataModel (fn (currentDataModel model)) model


currentDataModel : Model -> DataModel
currentDataModel model =
    Pivot.getC model.dataModelPivot


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
        |> jpHardcoded 0
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
    , centralOctave : Octave
    , startPitchClass : Int
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


minStartPitchClass =
    0


maxStartPitchClass =
    11


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


type MusicScale
    = Major
    | Chromatic


scaleFromString : String -> Maybe MusicScale
scaleFromString s =
    case s of
        "Major" ->
            Just Major

        "Chromatic" ->
            Just Chromatic

        _ ->
            Nothing


scaleToString : MusicScale -> String
scaleToString musicScale =
    case musicScale of
        Major ->
            "Major"

        Chromatic ->
            "Chromatic"


majorScaleToneIntervals : List ToneInterval
majorScaleToneIntervals =
    [ FullTone, FullTone, HalfTone, FullTone, FullTone, FullTone ]


musicScaleLength : MusicScale -> Int
musicScaleLength ms =
    case ms of
        Major ->
            7

        Chromatic ->
            12


pitchClassesForScaleStartingAt : MusicScale -> Int -> List Int
pitchClassesForScaleStartingAt musicScale startPitchClass =
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
                ( startPitchClass, [ startPitchClass ] )
                majorScaleToneIntervals
                |> second

        Chromatic ->
            List.range 0 11
                |> List.map (add startPitchClass)


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


initialSettingsV1 : Settings
initialSettingsV1 =
    { bars = 4
    , beatsPerBar = 4
    , beatSplits = 2
    , scale = Major
    , centralOctave = Mid
    , startPitchClass = 0
    , octaveRange = 2
    }


type PlayerState
    = Playing Float
    | NotPlaying


type Tool
    = Pen
    | Eraser


type GridType
    = InstrumentGrid
    | PercussionGrid


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    let
        dataModel =
            dataModelFromUrl url
    in
    ( { dataModelPivot = Pivot.singleton dataModel
      , stepIndex = 0
      , playState = NotPlaying
      , transientState = None
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


stepDurationInMilli : DataModel -> Float
stepDurationInMilli model =
    let
        beatDurationInMilli =
            (60 * 1000) / toFloat model.tempo

        duration =
            beatDurationInMilli / toFloat model.settings.beatSplits
    in
    duration


scheduleInstrumentNotesFromYS : Float -> DataModel -> List Int -> Cmd msg
scheduleInstrumentNotesFromYS audioTime model ys =
    instrumentNotesFromYS audioTime model ys
        |> scheduleNotes


instrumentNotesFromYS : Float -> DataModel -> List Int -> List Note
instrumentNotesFromYS audioTime model ys =
    instrumentPitchesFromYS model.settings ys
        |> List.map (instrumentNoteFromPitch audioTime model)


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
        pitchClasses =
            pitchClassesForScaleStartingAt settings.scale settings.startPitchClass

        pitchesForOctaveNum o =
            List.map (\n -> (12 * o) + n |> fromInt) pitchClasses
    in
    midiOctaveNumbers settings.centralOctave settings.octaveRange
        |> List.concatMap pitchesForOctaveNum


midiOctaveNumbers : Octave -> Int -> List Int
midiOctaveNumbers centralOctave octaveRange =
    let
        start =
            centralOctaveNumber centralOctave octaveRange

        end =
            start + octaveRange - 1
    in
    List.range start end
        -- NOTE: midi octaves start from -1
        |> List.map (add 1)


centralOctaveNumber : Octave -> Int -> Int
centralOctaveNumber centralOctave octaveRange =
    case octaveRange of
        1 ->
            octaveToInt centralOctave

        _ ->
            octaveToInt centralOctave - 1


instrumentNoteFromPitch : Float -> DataModel -> String -> Note
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


percussionNoteFromGP : Float -> DataModel -> Int2 -> Note
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
    | PointerUpOnGrid
    | OnBrowserKeyDown KeyEvent
    | OnAudioContextTime Float
      -- Bottom Bar
    | TogglePlayClicked
    | InstrumentButtonClicked
    | PercussionButtonClicked
    | TempoInputChanged String
    | SettingsClicked
    | UndoClicked
    | SaveClicked
      -- Settings Dialog
    | CloseSettingsClicked
    | SaveSettingsClicked
    | BarCountChanged String
    | BeatsPerBarChanged String
    | BeatSplitsChanged String
    | ScaleChanged String
    | CentralOctaveChanged String
    | StartPitchClassChanged String
    | OctaveRangeChanged String


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ onBrowserKeyDown OnBrowserKeyDown
    , onAudioContextTime OnAudioContextTime
    ]
        |> Sub.batch


playNoteIfDrawingCmd : Model -> GridType -> Tool -> Int2 -> Cmd msg
playNoteIfDrawingCmd model gridType tool gp =
    case tool of
        Pen ->
            playNoteCmd model gridType gp

        Eraser ->
            Cmd.none


playNoteCmd : Model -> GridType -> Int2 -> Cmd msg
playNoteCmd model gridType (( _, y ) as gp) =
    case gridType of
        InstrumentGrid ->
            scheduleInstrumentNotesFromYS model.audioTime (currentDataModel model) [ y ]

        PercussionGrid ->
            scheduleNote (percussionNoteFromGP model.audioTime (currentDataModel model) gp)



--playInstrumentNoteAtGPCmd : Model -> Int2 -> Cmd msg
--playInstrumentNoteAtGPCmd model ( _, y ) =
--    scheduleInstrumentNotesFromYS model.audioTime model.dataModel [ y ]
--
--
--playPercussionNoteAtGPCmd : Model -> Int2 -> Cmd msg
--playPercussionNoteAtGPCmd model gp =
--    scheduleNote (percussionNoteFromGP model.audioTime model.dataModel gp)


focusOrIgnoreCmd : String -> Cmd Msg
focusOrIgnoreCmd id =
    Browser.Dom.focus id
        |> Task.attempt (always NOP)


scheduleCurrentStepAtEffect : Float -> Model -> Cmd Msg
scheduleCurrentStepAtEffect atAudioTime ({ stepIndex } as model) =
    let
        dataModel =
            currentDataModel model
    in
    [ let
        igh =
            instrumentGridHeight dataModel.settings
      in
      dataModel.instrumentPositions
        |> Set.toList
        |> keep (first >> eq stepIndex)
        |> reject (second >> (\y -> y >= igh))
        |> List.map second
        |> scheduleInstrumentNotesFromYS atAudioTime dataModel
    , dataModel.percussionPositions
        |> Set.filter (first >> eq stepIndex)
        |> Set.toList
        |> List.map (percussionNoteFromGP atAudioTime dataModel >> scheduleNote)
        |> Cmd.batch
    ]
        |> Cmd.batch


updateBrowserUrlEffect : Model -> Cmd msg
updateBrowserUrlEffect model =
    let
        new =
            dataModelEncoderV2 (currentDataModel model) |> JE.encode 0

        old =
            payloadStringFromUrl model.url
    in
    if new /= old then
        Browser.Navigation.pushUrl model.key new

    else
        Cmd.none


setDrawState : Tool -> GridType -> Model -> Model
setDrawState tool gridType model =
    { model | transientState = Drawing tool gridType }


isPainted : Int2 -> GridType -> DataModel -> Bool
isPainted gp gt model =
    Set.member gp (getPaintedPositions gt model)


getPaintedPositions : GridType -> DataModel -> PaintedPositions
getPaintedPositions gridType =
    case gridType of
        InstrumentGrid ->
            .instrumentPositions

        PercussionGrid ->
            .percussionPositions


setPosition : Int2 -> Tool -> GridType -> DataModel -> DataModel
setPosition gp tool =
    case tool of
        Pen ->
            paintPosition gp

        Eraser ->
            erasePosition gp


paintPosition : Int2 -> GridType -> DataModel -> DataModel
paintPosition gp gridType =
    mapPositions (Set.insert gp) gridType


erasePosition : Int2 -> GridType -> DataModel -> DataModel
erasePosition gp gridType =
    mapPositions (Set.remove gp) gridType


mapPositions : (PaintedPositions -> PaintedPositions) -> GridType -> DataModel -> DataModel
mapPositions fn gt =
    case gt of
        InstrumentGrid ->
            mapInstrumentPositions fn

        PercussionGrid ->
            mapPercussionPositions fn


mapInstrumentPositions : (PaintedPositions -> PaintedPositions) -> DataModel -> DataModel
mapInstrumentPositions fn model =
    { model | instrumentPositions = fn model.instrumentPositions }


mapPercussionPositions : (PaintedPositions -> PaintedPositions) -> DataModel -> DataModel
mapPercussionPositions fn model =
    { model | percussionPositions = fn model.percussionPositions }


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
                    ( pushDataModel dataModel { model | url = url }, Cmd.none )

        PointerDownOnGP gridType gp ->
            let
                tool =
                    if isPainted gp gridType (currentDataModel model) then
                        Eraser

                    else
                        Pen
            in
            model
                |> mapPushDataModel (setPosition gp tool gridType)
                |> setDrawState tool gridType
                |> withCmd (playNoteIfDrawingCmd model gridType tool gp)

        PointerEnteredGP gridType gp ->
            case currentTool gridType model.transientState of
                Nothing ->
                    ( model, Cmd.none )

                Just tool ->
                    model
                        |> mapPushDataModel (setPosition gp tool gridType)
                        |> withCmd (playNoteIfDrawingCmd model gridType tool gp)

        PointerUpOnGrid ->
            ( case model.transientState of
                Drawing _ _ ->
                    { model | transientState = None }

                _ ->
                    model
            , Cmd.none
            )

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
            ( mapPushDataModel
                (\dataModel ->
                    { dataModel | instrument = cycleInstrument dataModel.instrument }
                )
                model
            , Cmd.none
            )

        PercussionButtonClicked ->
            ( mapPushDataModel
                (\dataModel ->
                    { dataModel | percussion = cyclePercussion dataModel.percussion }
                )
                model
            , Cmd.none
            )

        TempoInputChanged str ->
            ( mapPushDataModel
                (\dataModel ->
                    let
                        tempo =
                            String.toInt str
                                |> Maybe.withDefault dataModel.tempo
                                |> clamp 10 300
                    in
                    { dataModel | tempo = tempo }
                )
                model
            , Cmd.none
            )

        SettingsClicked ->
            ( { model | transientState = SettingsDialog (currentDataModel model).settings }
            , focusOrIgnoreCmd "cancel-settings-btn"
            )

        UndoClicked ->
            ( { model
                | dataModelPivot = withRollback Pivot.goL model.dataModelPivot
              }
            , Cmd.none
            )

        SaveClicked ->
            ( model, Cmd.none )

        CloseSettingsClicked ->
            ( { model | transientState = None }
            , focusOrIgnoreCmd "settings-btn"
            )

        SaveSettingsClicked ->
            case model.transientState of
                SettingsDialog newSettings ->
                    ( { model | transientState = None }
                        |> mapPushDataModel
                            (\dataModel ->
                                { dataModel
                                    | settings = newSettings
                                    , instrumentPositions =
                                        remapXPositions
                                            dataModel.settings
                                            newSettings
                                            dataModel.instrumentPositions
                                            |> remapYPositions
                                                dataModel.settings
                                                newSettings
                                    , percussionPositions =
                                        remapXPositions
                                            dataModel.settings
                                            newSettings
                                            dataModel.percussionPositions
                                }
                            )
                    , focusOrIgnoreCmd "settings-btn"
                    )

                _ ->
                    ( model, Cmd.none )

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

        ScaleChanged str ->
            updateSettingsForm2
                (\v s -> { s | scale = v })
                (scaleFromString str)
                model

        CentralOctaveChanged str ->
            updateSettingsForm2
                (\v s -> { s | centralOctave = v })
                (octaveFromString str)
                model

        StartPitchClassChanged str ->
            updateSettingsForm2
                (\v s -> { s | startPitchClass = v })
                (String.toInt str |> Maybe.map (clamp minStartPitchClass maxStartPitchClass))
                model

        OctaveRangeChanged str ->
            updateSettingsForm2
                (\octaveRange s -> { s | octaveRange = octaveRange })
                (String.toInt str |> Maybe.map (clamp minOctaveRange maxOctaveRange))
                model


updateSettingsForm2 : (a -> Settings -> Settings) -> Maybe a -> Model -> ( Model, Cmd msg )
updateSettingsForm2 fn maybe model =
    case ( model.transientState, maybe ) of
        ( SettingsDialog s, Just a ) ->
            { model | transientState = SettingsDialog (fn a s) } |> withNoCmd

        _ ->
            model |> withNoCmd


updateSettingsForm : (Settings -> Settings) -> Model -> ( Model, Cmd msg )
updateSettingsForm fn model =
    mapSettingsForm fn model |> withNoCmd


mapSettingsForm : (Settings -> Settings) -> Model -> Model
mapSettingsForm fn model =
    case model.transientState of
        SettingsDialog s ->
            { model | transientState = SettingsDialog (fn s) }

        _ ->
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
                    currentStepAudioTime + stepDurationInMilli (currentDataModel model)
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

                    dataModel =
                        currentDataModel model

                    nextStepAudioTime =
                        currentStepAudioTime + stepDurationInMilli dataModel
                in
                { model
                    | stepIndex = model.stepIndex + 1 |> modBy (totalSteps dataModel.settings)
                    , playState = Playing nextStepAudioTime
                }
                    |> withEffect (scheduleCurrentStepAtEffect currentStepAudioTime)

            else
                ( model, Cmd.none )


type alias BeatSplit =
    List Int


type alias Beat =
    ( ( BeatSplit, BeatSplit ), ( BeatSplit, BeatSplit ) )


emptyBeat : Beat
emptyBeat =
    ( ( [], [] ), ( [], [] ) )


type alias Bar =
    List Beat


remapXPositions : Settings -> Settings -> PaintedPositions -> PaintedPositions
remapXPositions from to =
    paintedPositionsToBars from
        >> resizeBarsToPaintedPositions to


remapYPositions : Settings -> Settings -> PaintedPositions -> PaintedPositions
remapYPositions from to pp =
    case ( from.scale, to.scale ) of
        ( Major, Chromatic ) ->
            Set.map (mapSecond majorToChromatic) pp

        ( Chromatic, Major ) ->
            setFilterMap (filterMapSecond chromaticToMajor)
                pp

        _ ->
            pp


chromaticOffsetsOfMajorScale : List Int
chromaticOffsetsOfMajorScale =
    [ 0, 2, 4, 5, 7, 9, 11 ]


chromaticToMajor : Int -> Maybe Int
chromaticToMajor y =
    let
        octaveOffset =
            y // 12

        chromaticScaleOffset =
            modBy 12 y
    in
    List.Extra.elemIndex chromaticScaleOffset chromaticOffsetsOfMajorScale
        |> Maybe.map (add (octaveOffset * 7))


majorToChromatic : Int -> Int
majorToChromatic y =
    let
        octaveOffset =
            y // 7

        majorScaleOffset =
            modBy 7 y

        chromaticScaleOffset =
            listGetAt majorScaleOffset chromaticOffsetsOfMajorScale
                |> Maybe.withDefault 0
    in
    octaveOffset * 12 + chromaticScaleOffset


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
    List.map (listPadRight emptyBeat settings.beatsPerBar) bars
        |> listPadRight [] settings.bars
        |> barsToPaintedPositions


viewDocument : Model -> Document Msg
viewDocument model =
    Document "Song Maker"
        [ basicStylesNode
        , animateCssNode
        , case model.transientState of
            SettingsDialog settings ->
                Html.Lazy.lazy viewSettingsForm settings

            Drawing _ _ ->
                view model

            None ->
                view model
        ]


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
        , Html.label []
            [ text "Scale: "
            , viewSelectLCR ScaleChanged
                (Pivot.fromCons Major [ Chromatic ]
                    |> withRollback (Pivot.firstWith (eq s.scale))
                    |> Pivot.mapA scaleToString
                    |> lcrFromPivot
                )
            ]
        , Html.label []
            [ text "Start on: "
            , viewSelectLCR CentralOctaveChanged
                (Pivot.fromCons Low [ Mid, High ]
                    |> withRollback (Pivot.firstWith (eq s.centralOctave))
                    |> Pivot.mapA octaveToString
                    |> lcrFromPivot
                )
            , viewSelectLCR2 StartPitchClassChanged
                (startPitchClassSelectLCR s.startPitchClass)
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


startPitchClassSelectLCR : Int -> LCR ( String, String )
startPitchClassSelectLCR startPitchClass =
    [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]
        |> List.indexedMap (\i t -> ( i, t ))
        |> List.drop 1
        |> Pivot.fromCons ( 0, "C" )
        |> withRollback (Pivot.firstWith (first >> eq startPitchClass))
        |> lcrFromPivot
        |> lcrMap (mapFirst fromInt)


viewBtn aa s =
    button
        ([ fontSize "20px"
         , pa "0.5ch 1ch"
         ]
            ++ aa
        )
        [ text s ]


viewSelectLCR msg lcr =
    Html.select [ fontSize "20px", onInput msg ]
        (lcrMapCS
            (\c -> Html.option [ HA.selected True ] [ text c ])
            (\s -> Html.option [] [ text s ])
            lcr
            |> lcrToList
        )


viewSelectLCR2 msg lcr =
    Html.select [ fontSize "20px", onInput msg ]
        (lcrMapCS
            (\( v, t ) -> Html.option [ HA.value v, HA.selected True ] [ text t ])
            (\( v, t ) -> Html.option [ HA.value v ] [ text t ])
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
    let
        dataModel =
            currentDataModel model
    in
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
            (instrumentName dataModel.instrument)
        , viewBtn
            [ sWidth "14ch"
            , notifyClick PercussionButtonClicked
            ]
            (percussionName dataModel.percussion)
        , viewTempoInput dataModel.tempo
        , viewSettingsButton
        , viewBtn
            [ notifyClick UndoClicked
            ]
            ("Undo " ++ fromInt (model.dataModelPivot |> Pivot.lengthL))
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
    let
        settings =
            (currentDataModel model).settings
    in
    fCol
        [ positionRelative
        , style "flex-grow" "1"
        , notifyPointerUp PointerUpOnGrid
        , noUserSelect
        ]
        [ div [ dGrid, positionRelative, style "flex-grow" "1" ]
            [ let
                w =
                    computeGridWidth settings

                h =
                    instrumentGridHeight settings
              in
              div [ dGrid, styleGridTemplate w h ]
                (rangeWH w h |> List.map (viewInstrumentTileAt model))
            , viewInstrumentGridLines settings
            ]
        , div [ dGrid, positionRelative, sHeight "20%" ]
            [ let
                w =
                    computeGridWidth settings

                h =
                    percussionGridHeight
              in
              div [ dGrid, styleGridTemplate w h ]
                (rangeWH w h |> List.map (viewPercussionTileAt model))
            , viewPercussionGridLines settings
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
        dataModel =
            currentDataModel model

        settings =
            dataModel.settings

        isPlaying =
            case model.playState of
                Playing _ ->
                    True

                _ ->
                    False

        isNoteTile =
            Set.member gp dataModel.percussionPositions

        isHighlightedTile =
            x == model.stepIndex

        anim =
            if isPlaying && isNoteTile && isHighlightedTile then
                blink

            else
                Animation.empty

        notesPerBar =
            settings.beatsPerBar * settings.beatSplits

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
        dataModel =
            currentDataModel model

        settings =
            dataModel.settings

        isPlaying =
            case model.playState of
                Playing _ ->
                    True

                _ ->
                    False

        isNoteTile =
            Set.member gp dataModel.instrumentPositions

        isHighlightedTile =
            x == model.stepIndex

        anim =
            if isPlaying && isNoteTile && isHighlightedTile then
                blink

            else
                Animation.empty

        notesPerBar =
            settings.beatsPerBar * settings.beatSplits

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
