port module SongMaker exposing (main)

import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Random
import Random.List
import Set exposing (Set)
import Url exposing (Url)
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


port play : List (List String) -> Cmd msg


port togglePlay : List (List String) -> Cmd msg


port playSingleNote : String -> Cmd msg


port updateSteps : List (List String) -> Cmd msg


port stop : () -> Cmd msg


port pause : () -> Cmd msg


port selectColumn : (Int -> msg) -> Sub msg


port stateChanged : (String -> msg) -> Sub msg


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


toNotesColumns : Int -> Set Int2 -> List (List String)
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

        columnToNotesDict : Dict Int (List String)
        columnToNotesDict =
            groupEqBy first (Set.toList pp)
                |> List.map (\( gp, gps ) -> ( first gp, List.map noteFromGP (gp :: gps) ))
                |> Dict.fromList
    in
    rangeN w
        |> List.map (\x -> Dict.get x columnToNotesDict |> Maybe.withDefault [])


noteFromGP : Int2 -> String
noteFromGP ( _, y ) =
    listGetAtWithDefault
        ""
        y
        [ "C4"
        , "D4"
        , "E4"
        , "F4"
        , "G4"
        , "A4"
        , "B4"
        , "C5"
        , "D5"
        , "E5"
        , "F5"
        , "G5"
        , "A5"
        , "B5"
        ]


noteColorFromGP : Int2 -> String
noteColorFromGP ( _, y ) =
    listGetAtWithDefault "" (modBy 7 y) colors


type Msg
    = NOP
    | PointerDownOnGP Int2
    | PointerEnteredGP Int2
    | OnPointerUp
    | PlayClicked
    | StopClicked
    | PauseClicked
    | ToggleClicked
    | SelectColumn Int
    | PlayerStateChanged String
    | OnKeyDown KeyEvent


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ selectColumn SelectColumn
    , stateChanged PlayerStateChanged
    , onBrowserKeyDown OnKeyDown
    ]
        |> Sub.batch


playEffect : Model -> Cmd msg
playEffect model =
    play (toNotesColumns model.w model.pp)


togglePlayEffect : Model -> Cmd msg
togglePlayEffect model =
    togglePlay (toNotesColumns model.w model.pp)


updateStepsEffect : Model -> Cmd msg
updateStepsEffect model =
    updateSteps (toNotesColumns model.w model.pp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        PointerDownOnGP gp ->
            (if Set.member gp model.pp then
                { model | pp = Set.remove gp model.pp, drawState = Just Erasing }
                    |> withNoCmd

             else
                { model | pp = Set.insert gp model.pp, drawState = Just Drawing }
                    |> withCmd (playSingleNote (noteFromGP gp))
            )
                |> addEffect updateStepsEffect

        PointerEnteredGP gp ->
            case model.drawState of
                Nothing ->
                    ( model, Cmd.none )

                Just Drawing ->
                    { model | pp = Set.insert gp model.pp }
                        |> withCmd (playSingleNote (noteFromGP gp))
                        |> addEffect updateStepsEffect

                Just Erasing ->
                    { model | pp = Set.remove gp model.pp }
                        |> withNoCmd
                        |> addEffect updateStepsEffect

        OnPointerUp ->
            ( { model | drawState = Nothing }, Cmd.none )

        PlayClicked ->
            model |> withEffect playEffect

        ToggleClicked ->
            model |> withEffect togglePlayEffect

        OnKeyDown e ->
            if e.isTargetBodyElement && not e.repeat && e.key == " " then
                model |> withEffect togglePlayEffect

            else if e.key == "s" then
                ( model
                , Browser.Navigation.replaceUrl model.key
                    (paintedPositionsEncoder model.pp |> JE.encode 0)
                )

            else
                ( model, Cmd.none )

        StopClicked ->
            ( model, stop () )

        PauseClicked ->
            ( model, pause () )

        SelectColumn cIdx ->
            ( { model | cIdx = cIdx }, Cmd.none )

        PlayerStateChanged playerStateString ->
            ( { model
                | playState =
                    case playerStateString of
                        "started" ->
                            Playing

                        _ ->
                            NotPlaying
              }
            , Cmd.none
            )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "Song Maker"
        [ basicStylesNode
        , view model
        ]


colors =
    [ wPink
    , wPurple
    , wBlue
    , wGreen2_sea
    , wOrange
    , wYellow
    , wGreen_lime
    ]


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
        , fRow [ gap "20px" ]
            [ fRow [ itemsCenter ] [ text ("Current Step: " ++ fromInt (model.cIdx + 1)) ]
            , fRow [ itemsCenter ] [ text ("Player State: " ++ Debug.toString model.playState) ]
            ]
        ]


viewPlayButton : PlayerState -> Html Msg
viewPlayButton playState =
    button
        [ autofocus True
        , fontSize "20px"
        , pa "0.5ch 1ch"
        , notifyClick ToggleClicked
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


computeTileColorAtGP : Model -> Int2 -> String
computeTileColorAtGP { pp, cIdx } (( x, _ ) as gp) =
    if Set.member gp pp then
        noteColorFromGP gp

    else if x == cIdx then
        hsl 0.6 0.2 0.4

    else if modBy 16 x >= 8 then
        hsl 0 0 0.1125

    else
        "transparent"


viewGrid : Model -> Html Msg
viewGrid ({ w, h } as model) =
    div [ dGrid, positionRelative, style "flex-grow" "1" ]
        [ viewGridTiles model
        , viewMinorGridLines w h
        , viewMajorGridLines w h
        ]


viewGridTiles : Model -> Html Msg
viewGridTiles ({ w, h } as model) =
    let
        tiles =
            rangeWH w h
                |> List.map (\gp -> viewTile (computeTileColorAtGP model gp) gp)
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


viewMinorGridLines w h =
    div
        [ w100
        , h100
        , positionAbsolute
        , noPointerEvents
        , backgroundImageForGridLines (hsl 0 0 0.16) "1px"
        , backgroundSizeForGridLinesWH w h
        ]
        []


viewMajorGridLines w h =
    div
        [ w100
        , h100
        , positionAbsolute
        , noPointerEvents
        , backgroundImages
            [ columnGridLineGradiant (hsl 0 0 0.3) "1px"
            , rowGridLineGradiant (hsl 0 0 0.3) "3px"
            ]
        , style "background-position" "-0.5px -1.5px"
        , backgroundSizeForGridLinesWH (w // 2) (h // 7)
        ]
        []


backgroundSizeForGridLinesWH w h =
    style "background-size"
        (fromFloat (100 / toFloat w) ++ "% " ++ (fromFloat (100 / toFloat h) ++ "%"))


backgroundImageForGridLines c t =
    backgroundImages
        [ columnGridLineGradiant c t
        , rowGridLineGradiant c t
        ]


columnGridLineGradiant c t =
    "linear-gradient(to right, " ++ c ++ " " ++ t ++ ", transparent 0)"


rowGridLineGradiant c t =
    "linear-gradient(to bottom, " ++ c ++ " " ++ t ++ ", transparent 0)"


viewTile c (( x, y ) as gp) =
    let
        ( row, col ) =
            ( y + 1, x + 1 )
    in
    div
        [ bgc c
        , style "grid-area" (fromInt row ++ "/" ++ fromInt col)
        , sMinHeight "20px"
        , notifyPointerDown (PointerDownOnGP gp)
        , notifyPointerEnter (PointerEnteredGP gp)
        ]
        []
