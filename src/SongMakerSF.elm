port module SongMakerSF exposing (main)

import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Html
import Html.Attributes as HA
import Html.Events exposing (onBlur)
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra
import Material.Icons
import Material.Icons.Types
import Pivot exposing (Pivot)
import Random
import Random.List
import Set exposing (Set)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg
import Svg.Attributes as SA
import Task
import Url exposing (Url)
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:
    Clone: Chrome Music Labs - Song Maker.

    # Next
    * [x] bottom bar visuals
    * [x] tempo input visuals
    * instrument/percussion backend
    * Save btn dialog
    * redo
    --
    * various instrument icons


    # Archived
    * [x] add all UI elements.
    * [x] make UI functional.
    * [x] implement features represented by UI.
        * [x] Instruments
        * [x] Tempo
        * [x] Settings
    * [x] Update player on tempo/settings change
    * [x] start noteName
    * [x] chromatic scale
    * [x] Undo
        * [x] reset on navigation
        * [x] introduce undo pivot ~~and explicitly add changes in every message~~.
        * [x] tempo: throttle / delay applying. for easier undo.
    * [x] percussion grid visuals
        * [x] pentatonic scale
        * [x] additional settings persistence
        * [x] chromatic scale colors
        * [x] percussion colors



-}


port scheduleNote : Note -> Cmd msg


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
    , playState : PlayState
    , transientState : TransientState
    , audioTime : Float
    , key : Key
    , url : Url
    }


type TransientState
    = SettingsDialog Settings
    | Drawing Tool GridType
    | EditTempo String
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


loadDataModel : DataModel -> Model -> Model
loadDataModel dataModel model =
    if dataModel /= currentDataModel model then
        { model
            | dataModelPivot = Pivot.singleton dataModel
            , transientState = None
            , stepIndex = 0
            , playState = NotPlaying
        }

    else
        model


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


paintedPositions : GridType -> DataModel -> PaintedPositions
paintedPositions gridType =
    case gridType of
        InstrumentGrid ->
            .instrumentPositions

        PercussionGrid ->
            .percussionPositions


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
        , ( "centralOctave", encodeOctave settings.centralOctave )
        , ( "scale", encodeMusicScale settings.scale )
        , ( "startPitchClass", JE.int settings.startPitchClass )
        , ( "octaveRange", JE.int settings.octaveRange )
        ]


encodeOctave : Octave -> Value
encodeOctave octave =
    case octave of
        Low ->
            JE.string "Low"

        Mid ->
            JE.string "Mid"

        High ->
            JE.string "High"


encodeMusicScale : MusicScale -> Value
encodeMusicScale musicScale =
    case musicScale of
        Major ->
            JE.string "Major"

        Chromatic ->
            JE.string "Chromatic"

        Pentatonic ->
            JE.string "Pentatonic"


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
dataModelFromPaintedPositionsV1 paintedPositions_ =
    let
        settings =
            initialSettingsV1

        igh =
            instrumentGridHeight settings
    in
    { instrumentPositions =
        paintedPositions_
            |> Set.filter (second >> (\y -> y < igh))
    , percussionPositions =
        paintedPositions_
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
        |> jpOptional "scale" musicScaleDecoder Major
        |> jpOptional "centralOctave" octaveDecoder Mid
        |> jpOptional "startPitchClass" JD.int 0
        |> jpOptional "octaveRange" JD.int 2


musicScaleDecoder : Decoder MusicScale
musicScaleDecoder =
    let
        get id =
            case id of
                "Major" ->
                    JD.succeed Major

                "Chromatic" ->
                    JD.succeed Chromatic

                "Pentatonic" ->
                    JD.succeed Pentatonic

                _ ->
                    JD.fail ("unknown value for MusicScale: " ++ id)
    in
    JD.string |> JD.andThen get


octaveDecoder : Decoder Octave
octaveDecoder =
    let
        get id =
            case id of
                "Low" ->
                    JD.succeed Low

                "Mid" ->
                    JD.succeed Mid

                "High" ->
                    JD.succeed High

                _ ->
                    JD.fail ("unknown value for Octave: " ++ id)
    in
    JD.string |> JD.andThen get


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


tempoInterval =
    ( 10, 300 )


clampInInterval ( lo, hi ) =
    clamp lo hi


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
    | Pentatonic


scaleFromString : String -> Maybe MusicScale
scaleFromString s =
    case s of
        "Major" ->
            Just Major

        "Pentatonic" ->
            Just Pentatonic

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

        Pentatonic ->
            "Pentatonic"


musicScaleLength : MusicScale -> Int
musicScaleLength ms =
    case ms of
        Major ->
            7

        Chromatic ->
            12

        Pentatonic ->
            5


pitchClassesForMusicScale : MusicScale -> List Int
pitchClassesForMusicScale musicScale =
    case musicScale of
        Major ->
            chromaticOffsetsOfMajorScale

        Chromatic ->
            List.range 0 11

        Pentatonic ->
            chromaticOffsetsOfPentatonicScale


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


type PlayState
    = Playing Float
    | NotPlaying


checkIfPlaying : PlayState -> Bool
checkIfPlaying playState =
    case playState of
        Playing _ ->
            True

        NotPlaying ->
            False


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
    , pitch : Int
    , duration : Float
    }


type alias AudioTimeAndDuration =
    { audioTime : Float
    , duration : Float
    }


initAudioTimeAndDuration : Float -> DataModel -> AudioTimeAndDuration
initAudioTimeAndDuration audioTime dataModel =
    { audioTime = audioTime, duration = stepDurationInMilli dataModel }


stepDurationInMilli : DataModel -> Float
stepDurationInMilli model =
    let
        beatDurationInMilli =
            (60 * 1000) / toFloat model.tempo

        duration =
            beatDurationInMilli / toFloat model.settings.beatSplits
    in
    duration


instrumentNoteAtIndex : AudioTimeAndDuration -> DataModel -> Int -> Note
instrumentNoteAtIndex audioTimeAndDuration dataModel =
    instrumentNoteAtIndexHelp
        audioTimeAndDuration
        dataModel.instrument
        (instrumentPitches dataModel.settings)


instrumentNotesAtIndices : AudioTimeAndDuration -> DataModel -> List Int -> List Note
instrumentNotesAtIndices audioTimeAndDuration dataModel =
    List.map
        (instrumentNoteAtIndexHelp
            audioTimeAndDuration
            dataModel.instrument
            (instrumentPitches dataModel.settings)
        )


instrumentNoteAtIndexHelp : AudioTimeAndDuration -> Instrument -> List Int -> Int -> Note
instrumentNoteAtIndexHelp audioTimeAndDuration instrument pitches index =
    instrumentPitchAtIndex pitches index
        |> initInstrumentNote audioTimeAndDuration (instrumentToPresetName instrument)


instrumentPitchAtIndex : List Int -> Int -> Int
instrumentPitchAtIndex pitches index =
    case listGetAt index pitches of
        Nothing ->
            Debug.todo "instrumentPitchesFromYS: invalid y"

        Just a ->
            a


instrumentPitches : Settings -> List Int
instrumentPitches settings =
    let
        pitchClasses : List Int
        pitchClasses =
            pitchClassesForMusicScale settings.scale
                |> List.map (add settings.startPitchClass)

        midiOctaveNumbers : List Int
        midiOctaveNumbers =
            computeMidiOctaveNumbers settings.centralOctave settings.octaveRange
    in
    instrumentPitchesHelp pitchClasses midiOctaveNumbers


instrumentPitchesHelp : List Int -> List Int -> List Int
instrumentPitchesHelp pitchClasses midiOctaveNumbers =
    let
        midiPitchesForMidiOctaveNumber octaveNumber =
            List.map
                (\pitchClass ->
                    (12 * octaveNumber) + pitchClass
                )
                pitchClasses
    in
    List.concatMap midiPitchesForMidiOctaveNumber midiOctaveNumbers


computeMidiOctaveNumbers : Octave -> Int -> List Int
computeMidiOctaveNumbers centralOctave octaveRange =
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


initInstrumentNote : AudioTimeAndDuration -> String -> Int -> Note
initInstrumentNote { audioTime, duration } preset pitch =
    { preset = preset
    , atAudioTime = audioTime
    , pitch = pitch
    , duration = duration
    }


instrumentToPresetName : Instrument -> String
instrumentToPresetName instrument =
    case instrument of
        Piano ->
            "piano"

        Strings ->
            "strings"

        _ ->
            "strings"


percussionNotesAtIndices : AudioTimeAndDuration -> DataModel -> List Int -> List Note
percussionNotesAtIndices audioTimeAndDuration dataModel =
    List.map (percussionNoteAtIndex audioTimeAndDuration dataModel)


percussionNoteAtIndex : AudioTimeAndDuration -> DataModel -> Int -> Note
percussionNoteAtIndex audioTimeAndDuration dataModel y =
    initNote
        audioTimeAndDuration
        (percussionPresetAndPitch dataModel.percussion y)


initNote : AudioTimeAndDuration -> PresetAndPitch -> Note
initNote { audioTime, duration } { preset, pitch } =
    { preset = preset
    , atAudioTime = audioTime
    , pitch = pitch
    , duration = duration
    }


percussionPresetAndPitch : Percussion -> Int -> PresetAndPitch
percussionPresetAndPitch percussion y =
    case ( percussion, y ) of
        ( Electronic, 0 ) ->
            snareDrum2

        ( Electronic, 1 ) ->
            bassDrum2

        ( Kit, 0 ) ->
            snareDrum1

        ( Kit, 1 ) ->
            bassDrum1

        --( Electronic, 0 ) -> snareDrum2
        --( Electronic, 1 ) -> bassDrum1
        ( Blocks, 0 ) ->
            highWoodBlock

        ( Blocks, 1 ) ->
            lowWoodBlock

        ( Conga, 0 ) ->
            openHighConga

        ( Conga, 1 ) ->
            lowConga

        _ ->
            Debug.todo (Debug.toString ( percussion, y ))


type alias PresetAndPitch =
    { preset : String
    , pitch : Int
    }


newPresetAndPitch : String -> Int -> PresetAndPitch
newPresetAndPitch preset pitch =
    { preset = preset, pitch = pitch }


snareDrum2 : PresetAndPitch
snareDrum2 =
    newPresetAndPitch "snareDrum2" 40


snareDrum1 : PresetAndPitch
snareDrum1 =
    newPresetAndPitch "snareDrum1" 38


bassDrum1 : PresetAndPitch
bassDrum1 =
    newPresetAndPitch "bassDrum1" 36


bassDrum2 : PresetAndPitch
bassDrum2 =
    newPresetAndPitch "bassDrum2" 35


highWoodBlock : PresetAndPitch
highWoodBlock =
    newPresetAndPitch "highWoodBlock" 76


lowWoodBlock : PresetAndPitch
lowWoodBlock =
    newPresetAndPitch "lowWoodBlock" 77


openHighConga : PresetAndPitch
openHighConga =
    newPresetAndPitch "openHighConga" 63


lowConga : PresetAndPitch
lowConga =
    newPresetAndPitch "lowConga" 64


noteColor : MusicScale -> Int2 -> String
noteColor musicScale ( _, y ) =
    let
        len =
            musicScaleLength musicScale

        idx =
            modBy (musicScaleLength musicScale) y

        pct =
            toFloat idx / toFloat len
    in
    noteColorHelp pct


noteColorHelp : Float -> String
noteColorHelp pct =
    "hsla(" ++ fromInt (pct |> mul 360 |> round |> add -15) ++ "deg 100% 60%)"


type Msg
    = NOP
    | UrlChanged Url
    | PointerDownOnGP GridType Int2
    | PointerEnteredGP GridType Int2
    | OnBrowserMouseUp
    | OnBrowserKeyDown KeyEvent
    | OnAudioContextTime Float
      -- Bottom Bar
    | TogglePlayClicked
    | InstrumentButtonClicked
    | PercussionButtonClicked
    | TempoInputChanged String
    | CommitTempoInput
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
    , Browser.Events.onMouseUp (JD.succeed OnBrowserMouseUp)
    ]
        |> Sub.batch


playNoteIfDrawingCmd : Model -> GridType -> Tool -> Int2 -> Cmd msg
playNoteIfDrawingCmd model gridType tool gp =
    case tool of
        Pen ->
            playNoteAtGPCmd model gridType gp

        Eraser ->
            Cmd.none


playNoteAtGPCmd : Model -> GridType -> Int2 -> Cmd msg
playNoteAtGPCmd model gridType ( _, y ) =
    noteAtIndex gridType model.audioTime (currentDataModel model) y
        |> scheduleNote


noteAtIndex : GridType -> Float -> DataModel -> Int -> Note
noteAtIndex gridType atAudioTime dataModel index =
    let
        audioTimeAndDuration =
            initAudioTimeAndDuration atAudioTime dataModel
    in
    case gridType of
        InstrumentGrid ->
            instrumentNoteAtIndex audioTimeAndDuration dataModel index

        PercussionGrid ->
            percussionNoteAtIndex audioTimeAndDuration dataModel index


focusOrIgnoreCmd : String -> Cmd Msg
focusOrIgnoreCmd id =
    Browser.Dom.focus id
        |> Task.attempt (always NOP)


scheduleNotesAtCurrentStepEffect : Float -> Model -> Cmd Msg
scheduleNotesAtCurrentStepEffect atAudioTime model =
    notesAtStep model.stepIndex atAudioTime (currentDataModel model)
        |> List.map scheduleNote
        |> Cmd.batch


notesAtStep : Int -> Float -> DataModel -> List Note
notesAtStep stepIndex atAudioTime dataModel =
    let
        audioTimeAndDuration : AudioTimeAndDuration
        audioTimeAndDuration =
            initAudioTimeAndDuration atAudioTime dataModel
    in
    gridNotesAtStep InstrumentGrid stepIndex audioTimeAndDuration dataModel
        ++ gridNotesAtStep PercussionGrid stepIndex audioTimeAndDuration dataModel


gridNotesAtStep : GridType -> Int -> AudioTimeAndDuration -> DataModel -> List Note
gridNotesAtStep gridType stepIndex audioTimeAndDuration dataModel =
    paintedPositionsToIndicesAtStep gridType dataModel stepIndex
        |> notesAtIndices gridType audioTimeAndDuration dataModel


notesAtIndices : GridType -> AudioTimeAndDuration -> DataModel -> List Int -> List Note
notesAtIndices gridType =
    case gridType of
        InstrumentGrid ->
            instrumentNotesAtIndices

        PercussionGrid ->
            percussionNotesAtIndices


paintedPositionsToIndicesAtStep : GridType -> DataModel -> Int -> List Int
paintedPositionsToIndicesAtStep gridType dataModel stepIndex =
    paintedPositions gridType dataModel
        |> paintedPositionsToIndicesAtStepHelp stepIndex


paintedPositionsToIndicesAtStepHelp : Int -> PaintedPositions -> List Int
paintedPositionsToIndicesAtStepHelp stepIndex positions =
    positions
        |> Set.toList
        |> keep (first >> eq stepIndex)
        |> List.map second


updateBrowserUrlEffect : Model -> Cmd msg
updateBrowserUrlEffect model =
    let
        new : String
        new =
            dataModelEncoderV2 (currentDataModel model) |> JE.encode 0

        old : String
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
            { model | url = url }
                |> (case decodeDataModelFromUrl url of
                        Err err ->
                            Debug.todo (JD.errorToString err)

                        Ok dataModel ->
                            loadDataModel dataModel >> withNoCmd
                   )

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

        OnBrowserMouseUp ->
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
                model |> withEffect updateBrowserUrlEffect

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
            ( { model | transientState = EditTempo str }
            , Cmd.none
            )

        CommitTempoInput ->
            ( case model.transientState of
                EditTempo str ->
                    { model | transientState = None }
                        |> mapPushDataModel
                            (\dataModel ->
                                let
                                    tempo =
                                        String.toInt str
                                            |> Maybe.withDefault dataModel.tempo
                                            |> clampInInterval tempoInterval
                                in
                                { dataModel | tempo = tempo }
                            )

                _ ->
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
                    scheduleDelay

                currentStepAudioTime =
                    model.audioTime + initialDelay

                nextStepAudioTime =
                    currentStepAudioTime + stepDurationInMilli (currentDataModel model)
            in
            { model
                | playState = Playing nextStepAudioTime
                , stepIndex = 0
            }
                |> withEffect (scheduleNotesAtCurrentStepEffect currentStepAudioTime)

        Playing _ ->
            { model | playState = NotPlaying } |> withNoCmd


scheduleDelay =
    100


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
            if diff <= scheduleDelay then
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
                    |> withEffect (scheduleNotesAtCurrentStepEffect currentStepAudioTime)

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
    paintedPositionsToBars from >> resizeBarsToPaintedPositions to


remapYPositions : Settings -> Settings -> PaintedPositions -> PaintedPositions
remapYPositions from to =
    toChromatic from.scale >> fromChromatic to.scale


toChromatic : MusicScale -> PaintedPositions -> PaintedPositions
toChromatic fromMusicScale =
    case fromMusicScale of
        Major ->
            Set.map (mapSecond majorToChromatic)

        Chromatic ->
            identity

        Pentatonic ->
            Set.map (mapSecond pentatonicToChromatic)


fromChromatic : MusicScale -> PaintedPositions -> PaintedPositions
fromChromatic toMusicScale =
    case toMusicScale of
        Major ->
            setFilterMap (filterMapSecond chromaticToMajor)

        Chromatic ->
            identity

        Pentatonic ->
            setFilterMap (filterMapSecond chromaticToPentatonic)


chromaticOffsetsOfMajorScale : List Int
chromaticOffsetsOfMajorScale =
    [ 0, 2, 4, 5, 7, 9, 11 ]


chromaticOffsetsOfPentatonicScale : List Int
chromaticOffsetsOfPentatonicScale =
    [ 0, 2, 4, 7, 9 ]


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


chromaticToPentatonic : Int -> Maybe Int
chromaticToPentatonic y =
    let
        octaveOffset =
            y // 12

        chromaticScaleOffset =
            modBy 12 y
    in
    List.Extra.elemIndex chromaticScaleOffset chromaticOffsetsOfPentatonicScale
        |> Maybe.map (add (octaveOffset * 5))


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


pentatonicToChromatic : Int -> Int
pentatonicToChromatic y =
    let
        octaveOffset =
            y // 5

        pentatonicScaleOffset =
            modBy 5 y

        chromaticScaleOffset =
            listGetAt pentatonicScaleOffset chromaticOffsetsOfPentatonicScale
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
        , styleNode """
            input,button{
                font-size:inherit;
                font-family:inherit;
            }
            button{
                cursor:pointer;
                user-select:none;
            }
        """
        , case model.transientState of
            SettingsDialog settings ->
                viewSettingsForm settings

            _ ->
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
                (Pivot.fromCons Major [ Pentatonic, Chromatic ]
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


iconButton : msg -> String -> List (Attribute msg) -> Html msg -> Html msg
iconButton msg labelText bAttrs iconEl =
    button
        ([ style "flex" "0 0 auto"
         , bgcTransparent
         , borderNone
         , fg white
         , styleWidth "10ch"
         , notifyClick msg
         , displayFlex
         , flexColumn
         , itemsCenter
         ]
            ++ bAttrs
        )
        [ iconButtonImageWrapper [ iconEl ], text labelText ]


iconButtonImageWrapper : List (Html msg) -> Html msg
iconButtonImageWrapper =
    let
        containerSizeInPx =
            fromInt 62 ++ "px"
    in
    div
        [ styleWidth containerSizeInPx
        , styleHeight containerSizeInPx
        , styleLineHeight containerSizeInPx
        , borderRadius50
        , style "border" ("1px solid " ++ grayN 0.3)
        , displayGrid
        , placeContentCenter
        ]


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
        [ fontSize "15px"
        , pa "10px"
        , gap "3ch"
        , itemsCenter
        , styleAccentColor wBlue
        ]
        [ viewPlayButton model.playState
        , iconButton InstrumentButtonClicked
            (instrumentName dataModel.instrument)
            []
            pianoIcon
        , iconButton PercussionButtonClicked
            (percussionName dataModel.percussion)
            []
            snareDrumIcon
        , viewTempoInput (tempoInputValue model)
        , iconButton SettingsClicked
            "Settings"
            [ HA.id "settings-btn" ]
            settingsIcon
        , iconButton UndoClicked
            ("Undo " ++ fromInt (model.dataModelPivot |> Pivot.lengthL))
            []
            undoIcon
        , iconButton NOP
            "Save"
            []
            saveIcon
        ]


tempoInputValue : Model -> ( String, Bool )
tempoInputValue model =
    case model.transientState of
        EditTempo str ->
            ( str, True )

        _ ->
            ( fromInt (currentDataModel model).tempo, False )


viewTempoInput : ( String, Bool ) -> Html Msg
viewTempoInput ( tempo, editing ) =
    Html.label
        [ style "flex" "1 1 auto"
        , displayFlex
        , flexRow
        , style "flex-wrap" "wrap"
        , gap "1ch"
        , contentCenter
        ]
        [ text "Tempo"
        , Html.input
            [ HA.value tempo
            , onInput TempoInputChanged
            , onBlur CommitTempoInput
            , onEnter CommitTempoInput
            , HA.type_ "range"
            , style "flex" "1 1 auto"
            , sMaxWidth "250px"

            --, style "flex" "1 0 auto"
            , HA.min (first tempoInterval |> fromInt)
            , HA.max (second tempoInterval |> fromInt)
            ]
            []
        , div [ displayFlex, flexRow ]
            [ div [ styleWidth "1ch" ] [ viewBool editing (text "*") ]
            , Html.input
                [ HA.value tempo
                , fg "inherit"
                , bgc "inherit"
                , tac
                , borderNone
                , onInput TempoInputChanged
                , onBlur CommitTempoInput
                , onEnter CommitTempoInput
                , HA.type_ "number"
                , HA.min (first tempoInterval |> fromInt)
                , HA.max (second tempoInterval |> fromInt)
                ]
                []
            ]
        ]


viewPlayButton : PlayState -> Html Msg
viewPlayButton playState =
    button
        [ autofocus True
        , notifyClick TogglePlayClicked
        , borderNone
        , bgc wBlue
        , borderRadius50
        , style "flex" "0 0 auto"
        , sWidth "80px"
        , sHeight "80px"
        , cursorPointer
        ]
        [ svg
            [ viewBoxC 100 100
            , fill "white"
            ]
            [ case playState of
                Playing _ ->
                    square 35 []

                NotPlaying ->
                    triangle 25 []
            ]
        ]


viewGrid : Model -> Html Msg
viewGrid model =
    let
        dataModel =
            currentDataModel model

        settings =
            dataModel.settings

        stepIndex =
            model.stepIndex

        isPlaying =
            checkIfPlaying model.playState

        instrumentPositions =
            dataModel.instrumentPositions

        percussionPositions =
            dataModel.percussionPositions
    in
    fCol
        [ positionRelative
        , style "flex-grow" "1"
        , noUserSelect
        ]
        [ Html.Lazy.lazy4 viewInstrumentGrid
            settings
            stepIndex
            isPlaying
            instrumentPositions
        , Html.Lazy.lazy4 viewPercussionGrid
            settings
            stepIndex
            isPlaying
            percussionPositions
        ]


viewPercussionGrid : Settings -> Int -> Bool -> Set ( Int, Int ) -> Html Msg
viewPercussionGrid settings stepIndex isPlaying percussionPositions =
    let
        ( gridWidth, gridHeight ) =
            ( computeGridWidth settings, percussionGridHeight )

        isTileAnimated gp =
            isPlaying && first gp == stepIndex

        viewPercussionTile_ gp =
            viewPercussionTile (isTileAnimated gp) gp
    in
    div [ displayGrid, positionRelative, sHeight "20%" ]
        [ viewGridBarBackground settings.bars
        , viewGridHighlightedColumnBackground gridWidth stepIndex
        , viewAbsoluteGridLayout gridWidth gridHeight [] <|
            (percussionPositions
                |> Set.toList
                |> List.map viewPercussionTile_
            )
        , viewPercussionGridLines settings
        , viewGridEventDispatcherTiles gridWidth gridHeight PercussionGrid
        ]


animatedSvg :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> Animation
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
animatedSvg =
    Animated.svg { class = SA.class }


viewPercussionTile : Bool -> Int2 -> Html Msg
viewPercussionTile isAnimated gp =
    let
        animContainer =
            if isAnimated then
                animatedSvg svg blink

            else
                svg

        bgColor =
            --noteColorFromGP gp
            wBlue
    in
    animContainer
        [ styleGridAreaFromGP gp
        , viewBoxC 100 100
        , style "place-self" "center"
        , style "padding" "10%"
        ]
        [ if second gp == 0 then
            circle 40 [ fill bgColor ]

          else
            triangle 50 [ fill bgColor, transforms [ rotateTurns -0.25 ] ]
        ]


viewInstrumentGrid : Settings -> Int -> Bool -> PaintedPositions -> Html Msg
viewInstrumentGrid settings stepIndex isPlaying instrumentPositions =
    let
        ( gridWidth, gridHeight ) =
            ( computeGridWidth settings, instrumentGridHeight settings )
    in
    let
        isTileAnimated gp =
            isPlaying && first gp == stepIndex

        viewInstrumentTile_ gp =
            viewInstrumentTile (isTileAnimated gp) (noteColor settings.scale gp) gp
    in
    div [ displayGrid, positionRelative, style "flex-grow" "1" ]
        [ viewGridBarBackground settings.bars
        , viewGridHighlightedColumnBackground gridWidth stepIndex
        , viewAbsoluteGridLayout gridWidth gridHeight [] <|
            (instrumentPositions
                |> Set.toList
                |> List.map viewInstrumentTile_
            )
        , viewInstrumentGridLines settings
        , viewGridEventDispatcherTiles gridWidth gridHeight InstrumentGrid
        ]


viewGridEventDispatcherTiles : Int -> Int -> GridType -> Html Msg
viewGridEventDispatcherTiles gridWidth gridHeight gridType =
    let
        viewEventDispatcherTile : Int2 -> Html Msg
        viewEventDispatcherTile gp =
            div
                [ styleGridAreaFromGP gp
                , notifyPointerDown (PointerDownOnGP gridType gp)
                , notifyPointerEnter (PointerEnteredGP gridType gp)
                ]
                []
    in
    viewAbsoluteGridLayout gridWidth gridHeight [] <|
        (rangeWH gridWidth gridHeight
            |> List.map viewEventDispatcherTile
        )


viewInstrumentTile : Bool -> String -> Int2 -> Html Msg
viewInstrumentTile isAnimated color gp =
    let
        animDiv =
            if isAnimated then
                Animated.div blink

            else
                div
    in
    animDiv [ bgc color, styleGridAreaFromGP gp ] []


viewGridHighlightedColumnBackground : Int -> Int -> Html msg
viewGridHighlightedColumnBackground gridWidth stepIndex =
    viewAbsoluteGridLayout gridWidth 1 [] <|
        [ div [ bgc highlightBGColor, styleGridAreaFromGP ( stepIndex, 0 ) ] [] ]


viewAbsoluteGridLayout : Int -> Int -> List (Attribute msg) -> List (Html msg) -> Html msg
viewAbsoluteGridLayout w h attrs =
    div ([ displayGrid, styleGridTemplate w h, positionAbsolute, w100, h100 ] ++ attrs)


viewGridBarBackground : Int -> Html msg
viewGridBarBackground bars =
    let
        w =
            bars

        h =
            1
    in
    div [ displayGrid, styleGridTemplate w h, positionAbsolute, w100, h100 ]
        ([ div [] [], div [ bgc barBGColor2 ] [] ]
            |> List.repeat (w // 2)
            |> List.concat
        )


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



-- SVG ICONS


pianoIcon : Html msg
pianoIcon =
    pianoIconSvg 32 wBlue



--noinspection ElmUnusedSymbol


electronicIcon : Html msg
electronicIcon =
    electronicIconSvg 62 wBlue


snareDrumIcon : Html msg
snareDrumIcon =
    snareDrumIconSvg 32 wBlue


settingsIcon : Html msg
settingsIcon =
    Material.Icons.settings 32 Material.Icons.Types.Inherit


undoIcon : Html msg
undoIcon =
    Material.Icons.undo 32 Material.Icons.Types.Inherit


saveIcon : Html msg
saveIcon =
    Material.Icons.check 32 Material.Icons.Types.Inherit



-- SVG ICONS


pianoIconSvg : Int -> String -> Html msg
pianoIconSvg size fillValue =
    svg
        [ viewBoxLT 48 48
        , styleWidth (fromInt size)
        , styleHeight (fromInt size)
        , fill fillValue
        ]
        [ Svg.path [ SA.d pianoIconSvgPath ] []
        ]


pianoIconSvgPath : String
pianoIconSvgPath =
    "M39,16h-8c-3.859,0-7-3.141-7-7c0-4.963-4.038-9-9-9H9C4.038,0,0,4.037,0,9v38h2v1h44.002v-1H48V25 C48,20.037,43.963,16,39,16z M8,46H4V30h3v9h1V46z M14,46h-4v-7h1v-9h2v9h1V46z M20,46h-4v-7h1v-9h3V46z M26,46h-3.999V30H25v9h1V46 z M32,46h-4v-7h1v-9h2v9h1V46z M38,46h-4v-7h1v-9h2v9h1V46z M44.002,46H40v-7h1v-9h3.002V46z M46,28H2V9c0-3.859,3.14-7,7-7h6 c3.86,0,7,3.141,7,7c0,4.963,4.037,9,9,9h8c3.859,0,7,3.141,7,7V28z"


electronicIconSvg : Int -> String -> Html msg
electronicIconSvg size fillValue =
    svg
        (viewBoxLT 60 60
            :: styleWidth (fromInt size)
            :: styleHeight (fromInt size)
            :: fill fillValue
            :: []
        )
        [ Svg.path
            [ style "transform" "translate(13px, 18px)"
            , SA.d electronicIconSvgPath
            ]
            []
        ]


snareDrumIconSvg : Int -> String -> Html msg
snareDrumIconSvg size fillValue =
    svg
        [ viewBoxLT 48 48
        , styleWidth (fromInt size)
        , styleHeight (fromInt size)
        , fill fillValue
        ]
        [ Svg.path [ SA.d snareDrumIconSvgPath ] []
        ]


snareDrumIconSvgPath : String
snareDrumIconSvgPath =
    "M48,27v-3c0-1.654-1.346-3-3-3h-7.661c0.134-0.084,0.257-0.182,0.368-0.293c0.575-0.574,0.793-1.379,0.598-2.205 c-0.341-1.441-1.831-2.731-3.988-3.451c-1.338-0.445-1.338-0.445-3.452-2.09c-0.771-0.601-1.847-1.436-3.359-2.584 c2.637-1.668,5.927-3.796,10.05-6.545C37.833,3.646,38,3.334,38,3c0-1.654-1.346-3-3-3c-0.208,0-0.412,0.064-0.581,0.186 C29.968,3.365,26.592,5.83,24,7.751c-2.591-1.921-5.968-4.386-10.419-7.565C13.412,0.064,13.208,0,13,0c-1.654,0-3,1.346-3,3 c0,0.334,0.167,0.646,0.445,0.832c4.124,2.749,7.413,4.877,10.05,6.545c-1.513,1.148-2.588,1.983-3.359,2.584 c-2.115,1.645-2.115,1.645-3.453,2.09c-2.157,0.72-3.648,2.01-3.988,3.451c-0.195,0.826,0.023,1.631,0.598,2.205 c0.111,0.111,0.235,0.209,0.368,0.293H3c-1.654,0-3,1.346-3,3v3h2v15H0v3c0,1.654,1.346,3,3,3h42c1.654,0,3-1.346,3-3v-3h-2V27H48z M33.684,16.949c1.885,0.627,2.567,1.561,2.675,2.013c0.051,0.215-0.025,0.29-0.065,0.331c-0.028,0.028-0.14,0.082-0.355,0.082 c-0.866,0-2.362-0.83-2.984-2.678C33.163,16.773,33.4,16.854,33.684,16.949z M35.27,2.037c0.269,0.074,0.491,0.26,0.618,0.502 c-4.187,2.785-7.481,4.908-10.091,6.551c-0.041-0.03-0.08-0.061-0.121-0.091C28.129,7.185,31.251,4.909,35.27,2.037z M12.112,2.539 c0.127-0.242,0.35-0.428,0.619-0.502c4.019,2.872,7.14,5.147,9.594,6.962c-0.041,0.03-0.081,0.061-0.122,0.091 C19.594,7.447,16.299,5.324,12.112,2.539z M11.642,18.962c0.106-0.452,0.79-1.386,2.674-2.013c0.283-0.095,0.52-0.176,0.73-0.252 c-0.622,1.847-2.118,2.678-2.984,2.678c-0.216,0-0.327-0.054-0.355-0.082C11.667,19.252,11.591,19.177,11.642,18.962z M16.948,17.317c0.236-0.704,0.236-0.704,2.448-1.999c1.018-0.596,2.497-1.463,4.604-2.756c2.106,1.293,3.586,2.16,4.604,2.756 c2.213,1.295,2.213,1.295,2.448,1.998c0.626,1.879,1.927,3.111,3.254,3.684H13.694C15.022,20.428,16.322,19.195,16.948,17.317z M46,45c0,0.551-0.448,1-1,1H3c-0.551,0-1-0.449-1-1v-1h44V45z M15,29v-2h18v2h-2v11h2v2H15v-2h2V29H15z M35,29v-2h9v15h-9v-2h2V29 H35z M35,38h-2v-7h2V38z M15,38h-2v-7h2V38z M13,29h-2v11h2v2H4V27h9V29z M2,25v-1c0-0.551,0.449-1,1-1h42c0.552,0,1,0.449,1,1v1H2z "


electronicIconSvgPath : String
electronicIconSvgPath =
    "M31.4,0.0222222222 L0.655555556,0.0222222222 C0.322222222,0.0222222222 0.0555555556,0.288888889 0.0555555556,0.622222222 L0.0555555556,23.7111111 C0.0555555556,24.0444444 0.322222222,24.3111111 0.655555556,24.3111111 L31.4,24.3111111 C31.7333333,24.3111111 32,24.0444444 32,23.7111111 L32,0.622222222 C32,0.288888889 31.7333333,0.0222222222 31.4,0.0222222222 Z M13.0888889,2.84444444 C13.8555556,2.84444444 14.4666667,3.46666667 14.4666667,4.22222222 C14.4666667,4.97777778 13.8444444,5.6 13.0888889,5.6 C12.3333333,5.6 11.7111111,4.97777778 11.7111111,4.22222222 C11.7111111,3.46666667 12.3222222,2.84444444 13.0888889,2.84444444 Z M9.16666667,2.84444444 C9.93333333,2.84444444 10.5444444,3.46666667 10.5444444,4.22222222 C10.5444444,4.97777778 9.92222222,5.6 9.16666667,5.6 C8.4,5.6 7.78888889,4.97777778 7.78888889,4.22222222 C7.78888889,3.46666667 8.41111111,2.84444444 9.16666667,2.84444444 Z M5.25555556,2.84444444 C6.02222222,2.84444444 6.63333333,3.46666667 6.63333333,4.22222222 C6.63333333,4.97777778 6.01111111,5.6 5.25555556,5.6 C4.48888889,5.6 3.87777778,4.97777778 3.87777778,4.22222222 C3.87777778,3.46666667 4.48888889,2.84444444 5.25555556,2.84444444 Z M6.65555556,20.2666667 C6.65555556,20.6 6.38888889,20.8666667 6.05555556,20.8666667 L4.28888889,20.8666667 C3.95555556,20.8666667 3.68888889,20.6 3.68888889,20.2666667 L3.68888889,17.9777778 C3.68888889,17.6444444 3.95555556,17.3777778 4.28888889,17.3777778 L6.05555556,17.3777778 C6.38888889,17.3777778 6.65555556,17.6444444 6.65555556,17.9777778 L6.65555556,20.2666667 Z M6.65555556,15.4 C6.65555556,15.7333333 6.38888889,16 6.05555556,16 L4.28888889,16 C3.95555556,16 3.68888889,15.7333333 3.68888889,15.4 L3.68888889,13.1111111 C3.68888889,12.7777778 3.95555556,12.5111111 4.28888889,12.5111111 L6.05555556,12.5111111 C6.38888889,12.5111111 6.65555556,12.7777778 6.65555556,13.1111111 L6.65555556,15.4 Z M11,20.2666667 C11,20.6 10.7333333,20.8666667 10.4,20.8666667 L10.3888889,20.8666667 L8.62222222,20.8666667 L8.61111111,20.8666667 C8.27777778,20.8666667 8.01111111,20.6 8.01111111,20.2666667 L8.01111111,17.9777778 C8.01111111,17.6444444 8.27777778,17.3777778 8.61111111,17.3777778 L8.62222222,17.3777778 L10.3888889,17.3777778 L10.4,17.3777778 C10.7333333,17.3777778 11,17.6444444 11,17.9777778 L11,20.2666667 Z M11,15.4 C11,15.7333333 10.7333333,16 10.4,16 L10.3888889,16 L8.62222222,16 L8.61111111,16 C8.27777778,16 8.01111111,15.7333333 8.01111111,15.4 L8.01111111,13.1111111 C8.01111111,12.7777778 8.27777778,12.5111111 8.61111111,12.5111111 L8.62222222,12.5111111 L10.3888889,12.5111111 L10.4,12.5111111 C10.7333333,12.5111111 11,12.7777778 11,13.1111111 L11,15.4 Z M15.3444444,20.2666667 C15.3444444,20.6 15.0777778,20.8666667 14.7444444,20.8666667 L12.9777778,20.8666667 C12.6444444,20.8666667 12.3777778,20.6 12.3777778,20.2666667 L12.3777778,17.9777778 C12.3777778,17.6444444 12.6444444,17.3777778 12.9777778,17.3777778 L14.7444444,17.3777778 C15.0777778,17.3777778 15.3444444,17.6444444 15.3444444,17.9777778 L15.3444444,20.2666667 Z M15.3444444,15.4 C15.3444444,15.7333333 15.0777778,16 14.7444444,16 L12.9777778,16 C12.6444444,16 12.3777778,15.7333333 12.3777778,15.4 L12.3777778,13.1111111 C12.3777778,12.7777778 12.6444444,12.5111111 12.9777778,12.5111111 L14.7444444,12.5111111 C15.0777778,12.5111111 15.3444444,12.7777778 15.3444444,13.1111111 L15.3444444,15.4 Z M19.7,20.2666667 C19.7,20.6 19.4333333,20.8666667 19.1,20.8666667 L17.3333333,20.8666667 C17,20.8666667 16.7333333,20.6 16.7333333,20.2666667 L16.7333333,17.9777778 C16.7333333,17.6444444 17,17.3777778 17.3333333,17.3777778 L19.1,17.3777778 C19.4333333,17.3777778 19.7,17.6444444 19.7,17.9777778 L19.7,20.2666667 Z M19.7,15.4 C19.7,15.7333333 19.4333333,16 19.1,16 L17.3333333,16 C17,16 16.7333333,15.7333333 16.7333333,15.4 L16.7333333,13.1111111 C16.7333333,12.7777778 17,12.5111111 17.3333333,12.5111111 L19.1,12.5111111 C19.4333333,12.5111111 19.7,12.7777778 19.7,13.1111111 L19.7,15.4 Z M19.6111111,9.28888889 L17.8444444,9.28888889 C17.5444444,9.75555556 17.0222222,10.0666667 16.4222222,10.0666667 C15.8222222,10.0666667 15.3,9.75555556 15,9.28888889 L4.55555556,9.28888889 C4.17777778,9.28888889 3.86666667,8.97777778 3.86666667,8.6 C3.86666667,8.22222222 4.17777778,7.91111111 4.55555556,7.91111111 L14.8111111,7.91111111 C15.0222222,7.22222222 15.6666667,6.71111111 16.4222222,6.71111111 C17.1777778,6.71111111 17.8222222,7.21111111 18.0333333,7.91111111 L19.6111111,7.91111111 C19.9888889,7.91111111 20.3,8.22222222 20.3,8.6 C20.3,8.97777778 20,9.28888889 19.6111111,9.28888889 Z M24.0555556,20.2666667 C24.0555556,20.6 23.7888889,20.8666667 23.4555556,20.8666667 L21.6888889,20.8666667 C21.3555556,20.8666667 21.0888889,20.6 21.0888889,20.2666667 L21.0888889,17.9777778 C21.0888889,17.6444444 21.3555556,17.3777778 21.6888889,17.3777778 L23.4555556,17.3777778 C23.7888889,17.3777778 24.0555556,17.6444444 24.0555556,17.9777778 L24.0555556,20.2666667 Z M24.0555556,15.4 C24.0555556,15.7333333 23.7888889,16 23.4555556,16 L21.6888889,16 C21.3555556,16 21.0888889,15.7333333 21.0888889,15.4 L21.0888889,13.1111111 C21.0888889,12.7777778 21.3555556,12.5111111 21.6888889,12.5111111 L23.4555556,12.5111111 C23.7888889,12.5111111 24.0555556,12.7777778 24.0555556,13.1111111 L24.0555556,15.4 Z M28.4111111,20.2666667 C28.4111111,20.6 28.1444444,20.8666667 27.8111111,20.8666667 L26.0444444,20.8666667 C25.7111111,20.8666667 25.4444444,20.6 25.4444444,20.2666667 L25.4444444,17.9777778 C25.4444444,17.6444444 25.7111111,17.3777778 26.0444444,17.3777778 L27.8111111,17.3777778 C28.1444444,17.3777778 28.4111111,17.6444444 28.4111111,17.9777778 L28.4111111,20.2666667 Z M28.4111111,15.4 C28.4111111,15.7333333 28.1444444,16 27.8111111,16 L26.0444444,16 C25.7111111,16 25.4444444,15.7333333 25.4444444,15.4 L25.4444444,13.1111111 C25.4444444,12.7777778 25.7111111,12.5111111 26.0444444,12.5111111 L27.8111111,12.5111111 C28.1444444,12.5111111 28.4111111,12.7777778 28.4111111,13.1111111 L28.4111111,15.4 Z M25.3111111,9.34444444 C23.5,9.34444444 22.0333333,7.86666667 22.0333333,6.06666667 C22.0333333,4.26666667 23.5111111,2.78888889 25.3111111,2.78888889 C27.1222222,2.78888889 28.5888889,4.26666667 28.5888889,6.06666667 C28.5888889,7.86666667 27.1222222,9.34444444 25.3111111,9.34444444 Z"
