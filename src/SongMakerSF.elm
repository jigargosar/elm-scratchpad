port module SongMakerSF exposing (main)

import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html
import Html.Attributes as HA
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
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
    * implement features represented by UI.
        * Instruments
        * Tempo
        * Settings
        * Undo
        * Save
    * Update player on tempo/settings change




-}


port start : List (List Note) -> Cmd msg


port stop : () -> Cmd msg


port playSingleNote : Note -> Cmd msg


port playNote2 : Note2 -> Cmd msg


port updateSteps : List (List Note) -> Cmd msg


port selectColumn : (Int -> msg) -> Sub msg


main =
    browserApplication
        { init = init
        , onUrlRequest = always NOP
        , onUrlChange = always NOP
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    { pp : Set Int2
    , cIdx : Int
    , playState : PlayerState
    , drawState : Maybe DrawState
    , showSettings : Bool
    , settings : Settings
    , instrument1 : Instrument1
    , instrument2 : Instrument2
    , tempo : Int
    , now : Int
    , key : Key
    }


type Instrument1
    = Piano
    | Strings
    | Woodwind
    | Synth
    | Marimba


instrument1Name : Instrument1 -> String
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


cycleInstrument1 : Instrument1 -> Instrument1
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


type Instrument2
    = Electronic
    | Blocks
    | Kit
    | Conga


instrument2Name : Instrument2 -> String
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


cycleInstrument2 : Instrument2 -> Instrument2
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
    , octaves : Int
    }


computeGridWidth : Settings -> Int
computeGridWidth s =
    s.bars * s.beatsPerBar * s.beatSplits


computeGridHeight : Settings -> Int
computeGridHeight s =
    case s.scale of
        Major ->
            7 * s.octaves + 2


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
    , octaves = 2
    }


type PlayerState
    = Playing Int
    | NotPlaying


type DrawState
    = Drawing
    | Erasing


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
        initialPP : Set GPos
        initialPP =
            rangeWH w h
                |> Random.List.shuffle
                |> Random.andThen Random.List.shuffle
                |> stepWithInitialSeed 2
                |> List.take 30
                |> Set.fromList

        pp =
            url.path
                |> String.dropLeft 1
                |> Url.percentDecode
                |> Maybe.withDefault ""
                |> JD.decodeString paintedPositionsDecoder
                |> Result.withDefault initialPP
    in
    ( { pp = pp
      , cIdx = 0
      , playState = NotPlaying
      , drawState = Nothing
      , showSettings = False
      , settings = settings
      , instrument1 = Piano
      , instrument2 = Electronic
      , tempo = 120
      , now = 0
      , key = key
      }
    , Cmd.none
    )


paintedPositionsDecoder : Decoder (Set GPos)
paintedPositionsDecoder =
    JD.map Set.fromList (JD.list (JD.map2 Tuple.pair (JD.index 0 JD.int) (JD.index 1 JD.int)))


paintedPositionsEncoder : Set GPos -> Value
paintedPositionsEncoder =
    JE.set (\( a, b ) -> JE.list identity [ JE.int a, JE.int b ])


toNotesColumns : Model -> List (List Note)
toNotesColumns model =
    let
        w =
            computeGridWidth model.settings

        columnToNotesDict : Dict Int (List Note)
        columnToNotesDict =
            groupEqBy first (Set.toList model.pp)
                |> List.map (\( gp, gps ) -> ( first gp, List.map (noteFromGP model) (gp :: gps) ))
                |> Dict.fromList
    in
    rangeN w
        |> List.map (\x -> Dict.get x columnToNotesDict |> Maybe.withDefault [])


type alias Note =
    ( String, String )


type alias Note2 =
    { inst : String
    , pitch : String
    , duration : Int
    , startOffset : Int
    }


noteFromGP : Model -> Int2 -> Note
noteFromGP model ( _, y ) =
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
        case model.instrument1 of
            Piano ->
                ( "piano", listGetAtOrDefault "" y noteNames )

            Strings ->
                ( "strings", listGetAtOrDefault "" y noteNames )

            _ ->
                ( "strings", listGetAtOrDefault "" y noteNames )

    else if y == 14 then
        case model.instrument2 of
            Electronic ->
                ( "snareDrum2", "40" )

            Blocks ->
                ( "snareDrum2", "40" )

            _ ->
                ( "snareDrum2", "40" )

    else if y == 15 then
        case model.instrument2 of
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
    | PointerDownOnGP Int2
    | PointerEnteredGP Int2
    | OnPointerUp
    | TogglePlayClicked
    | SettingsClicked
    | Instrument1ButtonClicked
    | Instrument2ButtonClicked
    | TempoInputChanged String
    | CloseSettingsClicked
    | SelectColumn Int
    | OnKeyDown KeyEvent


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ selectColumn SelectColumn
    , onBrowserKeyDown OnKeyDown
    ]
        |> Sub.batch


updateStepsEffect : Model -> Cmd msg
updateStepsEffect model =
    updateSteps (toNotesColumns model)


startPlayingEffect : Model -> Cmd msg
startPlayingEffect model =
    start (toNotesColumns model)


stopCmd : Cmd msg
stopCmd =
    stop ()


playSingleNoteCmd : Model -> Int2 -> Cmd msg
playSingleNoteCmd model gp =
    playSingleNote (noteFromGP model gp)


updateOnTogglePlay : Model -> ( Model, Cmd Msg )
updateOnTogglePlay model =
    case model.playState of
        NotPlaying ->
            { model | playState = Playing model.now } |> withEffect startPlayingEffect

        Playing _ ->
            { model | playState = NotPlaying } |> withCmd stopCmd


focusOrIgnoreCmd id =
    Browser.Dom.focus id
        |> Task.attempt (always NOP)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        PointerDownOnGP gp ->
            if Set.member gp model.pp then
                { model | pp = Set.remove gp model.pp, drawState = Just Erasing }
                    |> withEffect updateStepsEffect

            else
                { model | pp = Set.insert gp model.pp, drawState = Just Drawing }
                    |> withCmd (playSingleNoteCmd model gp)
                    |> addEffect updateStepsEffect

        PointerEnteredGP gp ->
            case model.drawState of
                Nothing ->
                    ( model, Cmd.none )

                Just Drawing ->
                    { model | pp = Set.insert gp model.pp }
                        |> withCmd (playSingleNoteCmd model gp)
                        |> addEffect updateStepsEffect

                Just Erasing ->
                    { model | pp = Set.remove gp model.pp }
                        |> withNoCmd
                        |> addEffect updateStepsEffect

        OnPointerUp ->
            ( { model | drawState = Nothing }, Cmd.none )

        TogglePlayClicked ->
            updateOnTogglePlay model

        SettingsClicked ->
            ( { model | showSettings = True }
            , focusOrIgnoreCmd "cancel-settings-btn"
            )

        Instrument1ButtonClicked ->
            ( { model | instrument1 = cycleInstrument1 model.instrument1 }
            , Cmd.none
            )

        Instrument2ButtonClicked ->
            ( { model | instrument2 = cycleInstrument2 model.instrument2 }
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

        CloseSettingsClicked ->
            ( { model | showSettings = False }, focusOrIgnoreCmd "settings-btn" )

        OnKeyDown e ->
            if e.isTargetBodyElement && not e.repeat && e.key == " " then
                updateOnTogglePlay model

            else if e.key == "s" then
                ( model
                , Browser.Navigation.replaceUrl model.key
                    (paintedPositionsEncoder model.pp |> JE.encode 0)
                )

            else
                ( model, Cmd.none )

        SelectColumn cIdx ->
            ( { model | cIdx = cIdx }, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "Song Maker"
        [ basicStylesNode
        , animateCssNode
        , if model.showSettings then
            viewSettings

          else
            view model
        ]


viewSettings : Html Msg
viewSettings =
    fCol [ pa "20px", gap "20px" ]
        [ div [ fontSize "22px" ] [ text "SETTINGS" ]
        , Html.label [] [ text "Length (in Bars): ", viewSelect [ "4" ] ]
        , Html.label [] [ text "Beats per bar: ", viewSelect [ "4" ] ]
        , Html.label [] [ text "Split beats into: ", viewSelect [ "2" ] ]
        , Html.label [] [ text "Scale: ", viewSelect [ "Major", "Minor", "Chromatic" ] ]
        , Html.label []
            [ text "Start on: "
            , viewSelect [ "Middle", "Low", "High" ]
            , viewSelect [ "C", "C#", "B" ]
            ]
        , Html.label [] [ text "Range: ", viewSelect [ "1", "2", "3" ] ]
        , fRow [ gap "20px" ]
            [ viewBtn [] "Ok"
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
            , notifyClick Instrument1ButtonClicked
            ]
            (instrument1Name model.instrument1)
        , viewBtn
            [ sWidth "14ch"
            , notifyClick Instrument2ButtonClicked
            ]
            (instrument2Name model.instrument2)
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
    div
        [ dGrid
        , positionRelative
        , style "flex-grow" "1"
        ]
        [ viewGridTiles model
        , viewGridLines model.settings
        ]


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
                , backgroundGridLinesVertical 2 (grayN 0.3) (2 / w)
                , backgroundGridLinesHorizontal 3 (grayN 0.3) (7 / h)
                ]
            )
        ]
        []


viewGridTiles : Model -> Html Msg
viewGridTiles model =
    let
        w =
            computeGridWidth model.settings

        h =
            computeGridHeight model.settings

        tiles =
            rangeWH w h
                |> List.map (viewTile model)
    in
    div
        [ dGrid
        , style "grid-template"
            (("repeat(" ++ fromInt h ++ ",1fr)")
                ++ "/"
                ++ ("repeat(" ++ fromInt w ++ ",1fr)")
            )
        , noUserSelect
        , notifyPointerUp OnPointerUp
        ]
        tiles


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


viewTile : Model -> Int2 -> Html Msg
viewTile model (( x, _ ) as gp) =
    let
        isPlaying =
            case model.playState of
                Playing _ ->
                    True

                _ ->
                    False

        isNoteTile =
            Set.member gp model.pp

        isHighlightedTile =
            x == model.cIdx

        anim =
            if isPlaying && isNoteTile && isHighlightedTile then
                blink

            else
                Animation.empty

        isAlternateBarTile =
            modBy 16 x >= 8

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
        , notifyPointerDown (PointerDownOnGP gp)
        , notifyPointerEnter (PointerEnteredGP gp)
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
        { startAt = [ P.opacity 1, P.scale 1 ]
        , options = []
        }
        [ Animation.step 50 [ P.opacity 0.2, P.scale 1.05 ]
        , let
            barLengthSec =
                2

            noteGapSec =
                (1 / 8) * barLengthSec

            noteGapMilli =
                noteGapSec * 1000 |> round
          in
          Animation.step noteGapMilli [ P.opacity 1, P.scale 1 ]
        ]
