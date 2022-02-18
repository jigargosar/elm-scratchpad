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
computeTileColorAtGP { pp, cIdx } gp =
    if Set.member gp pp then
        noteColorFromGP gp

    else if first gp == cIdx then
        hsl 0.6 0.2 0.4

    else
        "transparent"


viewGrid : Model -> Html Msg
viewGrid ({ w, h, pp, cIdx, playState } as model) =
    let
        tiles =
            rangeWH w h
                |> List.map (\gp -> viewTile (computeTileColorAtGP model gp) gp)

        hLines =
            List.range 1 (h - 1) |> List.map viewHLine

        vLines =
            List.range 1 (w - 1) |> List.map viewVLine
    in
    (tiles ++ hLines ++ vLines)
        |> div
            [ style "flex-grow" "1"
            , dGrid
            , style "grid-template"
                (("repeat(" ++ fromInt h ++ ",1fr)")
                    ++ "/"
                    ++ ("repeat(" ++ fromInt w ++ ",1fr)")
                )
            , noUserSelect
            , notifyPointerUp OnPointerUp
            ]


viewHLine y =
    div
        ([ style "grid-row" (fromInt y ++ "/" ++ fromInt (y + 2))
         , style "grid-column" "1/-1"
         , noPointerEvents
         , style "align-self" "center"
         ]
            ++ (if modBy 7 y == 0 then
                    [ bgc majorGridLineColor, sHeight majorGridLineThickness ]

                else
                    [ bgc minorGridLineColor, sHeight minorGridLineThickness ]
               )
        )
        []


minorGridLineColor =
    wGray


minorGridLineThickness =
    "1px"


majorGridLineColor =
    wLightGray


majorGridLineThickness =
    "2px"


viewVLine x =
    div
        ([ style "grid-column" (fromInt x ++ "/" ++ fromInt (x + 2))
         , style "grid-row" "1/-1"
         , noPointerEvents
         , style "justify-self" "center"
         ]
            ++ (if modBy 4 x == 0 then
                    [ bgc majorGridLineColor, sWidth majorGridLineThickness ]

                else
                    [ bgc minorGridLineColor, sWidth minorGridLineThickness ]
               )
        )
        []


viewTile c (( x, y ) as gp) =
    let
        ( row, col ) =
            ( y + 1, x + 1 )
    in
    div
        [ bgc c
        , style "grid-area" (fromInt row ++ "/" ++ fromInt col)

        --, sOutline ("0.5px solid " ++ wLightGray)
        , sMinHeight "20px"
        , notifyPointerDown (PointerDownOnGP gp)
        , notifyPointerEnter (PointerEnteredGP gp)
        ]
        []
