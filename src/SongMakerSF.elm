port module SongMakerSF exposing (main)

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
import Url exposing (Url)
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


port start : List (List Note) -> Cmd msg


port stop : () -> Cmd msg


port playSingleNote : Note -> Cmd msg


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
    { w : Int
    , h : Int
    , pp : Set Int2
    , cIdx : Int
    , playState : PlayerState
    , drawState : Maybe DrawState
    , showSettings : Bool
    , key : Key
    }


type PlayerState
    = Playing
    | NotPlaying


type DrawState
    = Drawing
    | Erasing


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    let
        w =
            32

        h =
            14
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
    ( { w = w
      , h = h
      , pp = pp
      , cIdx = 0
      , playState = NotPlaying
      , drawState = Nothing
      , showSettings = True
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


toNotesColumns : Int -> Set Int2 -> List (List Note)
toNotesColumns w pp =
    let
        _ =
            [ [ "C4" ]
            , [ "E4", "D4", "E4" ]
            , [ "G4" ]
            , [ "A4", "G4" ]
            ]
                |> List.concat
                |> List.map List.singleton

        columnToNotesDict : Dict Int (List Note)
        columnToNotesDict =
            groupEqBy first (Set.toList pp)
                |> List.map (\( gp, gps ) -> ( first gp, List.map noteFromGP (gp :: gps) ))
                |> Dict.fromList
    in
    rangeN w
        |> List.map (\x -> Dict.get x columnToNotesDict |> Maybe.withDefault [])


type alias Note =
    ( String, String )


noteFromGP : Int2 -> Note
noteFromGP ( _, y ) =
    listGetAtWithDefault
        ( "", "" )
        y
        [ ( "synth", "C3" )
        , ( "synth", "D3" )
        , ( "synth", "E3" )
        , ( "synth", "F3" )
        , ( "synth", "G3" )
        , ( "synth", "A3" )
        , ( "synth", "B3" )
        , ( "synth", "C4" )
        , ( "synth", "D4" )
        , ( "synth", "E4" )
        , ( "synth", "F4" )
        , ( "synth", "G4" )
        , ( "synth", "A4" )
        , ( "synth", "B4" )
        , ( "drum", "C1" )
        , ( "drum", "B1" )
        ]


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
    listGetAtWithDefault "" (modBy 7 y) colors


type Msg
    = NOP
    | PointerDownOnGP Int2
    | PointerEnteredGP Int2
    | OnPointerUp
    | TogglePlayClicked
    | SettingsClicked
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
    updateSteps (toNotesColumns model.w model.pp)


startPlayingEffect : Model -> Cmd msg
startPlayingEffect model =
    start (toNotesColumns model.w model.pp)


stopCmd : Cmd msg
stopCmd =
    stop ()


playSingleNoteCmd : Int2 -> Cmd msg
playSingleNoteCmd gp =
    playSingleNote (noteFromGP gp)


updateOnTogglePlay : Model -> ( Model, Cmd Msg )
updateOnTogglePlay model =
    case model.playState of
        NotPlaying ->
            { model | playState = Playing } |> withEffect startPlayingEffect

        Playing ->
            { model | playState = NotPlaying } |> withCmd stopCmd


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
                    |> withCmd (playSingleNoteCmd gp)
                    |> addEffect updateStepsEffect

        PointerEnteredGP gp ->
            case model.drawState of
                Nothing ->
                    ( model, Cmd.none )

                Just Drawing ->
                    { model | pp = Set.insert gp model.pp }
                        |> withCmd (playSingleNoteCmd gp)
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
            ( model, Cmd.none )

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
            view model

          else
            viewSettings
        ]


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
        , fRow [ gap "20px" ] [ viewButton "Ok", viewButton "Cancel" ]
        ]


viewButton s =
    button
        [ fontSize "20px"
        , pa "0.5ch 1ch"
        , notifyClick SettingsClicked
        ]
        [ text s ]


viewSelect l =
    Html.select [ fontSize "20px" ] (l |> List.map (\s -> Html.option [] [ text s ]))


view : Model -> Html Msg
view model =
    fCol []
        [ viewGrid model
        , viewBottomRow model
        ]


viewBottomRow : Model -> Html Msg
viewBottomRow model =
    fRow
        [ pa "20px"
        , gap "20px"
        , itemsCenter
        ]
        [ viewPlayButton model.playState
        , viewTempoInput
        , viewSettingsButton
        , viewUndoButton
        , viewSaveButton
        ]


viewSettingsButton =
    button
        [ fontSize "20px"
        , pa "0.5ch 1ch"
        , notifyClick SettingsClicked
        ]
        [ text "Settings" ]


viewUndoButton =
    button
        [ fontSize "20px"
        , pa "0.5ch 1ch"
        , notifyClick NOP
        ]
        [ text "Undo" ]


viewSaveButton =
    button
        [ fontSize "20px"
        , pa "0.5ch 1ch"
        , notifyClick NOP
        ]
        [ text "Save" ]


viewTempoInput =
    Html.label []
        [ text "Tempo: "
        , Html.input
            [ HA.value "120"
            , HA.size 4

            --, HA.type_ "number"
            , fontSize "20px"
            , sWidth "fit-content"
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
                    Playing ->
                        "Stop"

                    NotPlaying ->
                        "Play"
                )
            ]
        ]


viewGrid : Model -> Html Msg
viewGrid ({ w, h } as model) =
    div
        [ dGrid
        , positionRelative
        , style "flex-grow" "1"
        ]
        [ viewGridTiles model
        , viewGridLines w h
        ]


viewGridLines w_ h_ =
    let
        ( w, h ) =
            ( toFloat w_, toFloat h_ + 2 )
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
viewGridTiles ({ w, h } as model) =
    let
        tiles =
            rangeWH w (h + 2)
                |> List.map (viewTile model)
    in
    div
        [ dGrid
        , style "grid-template"
            (("repeat(" ++ fromInt (h + 2) ++ ",1fr)")
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
            model.playState == Playing

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
