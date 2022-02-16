port module SongMaker exposing (main)

import Dict exposing (Dict)
import Html exposing (button, span)
import Random
import Random.List
import Set exposing (Set)
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
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    { w : Int
    , h : Int
    , pp : Set Int2
    , cIdx : Int
    , playerState : String
    , drawState : Maybe DrawState
    }


type DrawState
    = Drawing
    | Erasing


init : () -> ( Model, Cmd Msg )
init () =
    let
        w =
            16

        h =
            14
    in
    let
        paintedPositions =
            rangeWH w h
                |> Random.List.shuffle
                |> Random.andThen Random.List.shuffle
                |> stepWithInitialSeed 2
                |> List.take 30
                |> Set.fromList

        --|> always Set.empty
    in
    ( { w = w
      , h = h
      , pp = paintedPositions
      , cIdx = 0
      , playerState = "unknown"
      , drawState = Nothing
      }
    , Cmd.none
    )


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
            if not e.repeat && e.key == " " then
                model |> withEffect togglePlayEffect

            else
                ( model, Cmd.none )

        StopClicked ->
            ( model, stop () )

        PauseClicked ->
            ( model, pause () )

        SelectColumn cIdx ->
            ( { model | cIdx = cIdx }, Cmd.none )

        PlayerStateChanged playerState ->
            ( { model | playerState = playerState }, Cmd.none )


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
view ({ w, h, pp } as model) =
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
        , fontSize "16px"
        ]
        [ button
            [ autofocus True
            , fontSize "20px"
            , pa "0.5ch 1ch"
            , notifyClick ToggleClicked
            , alwaysPreventDefaultOnKeyDown NOP
            ]
            [ span [ style "display" "inline-block", sMinWidth "4ch" ]
                [ text
                    (case model.playerState of
                        "stopped" ->
                            "Play"

                        "paused" ->
                            "Play"

                        _ ->
                            "Stop"
                    )
                ]
            ]
        , fCol []
            [ fRow [ itemsCenter ] [ text ("Current Step: " ++ fromInt (model.cIdx + 1)) ]
            , fRow [ itemsCenter ] [ text ("Player State: " ++ model.playerState) ]
            ]
        ]


viewGrid : Model -> Html Msg
viewGrid { w, h, pp, cIdx, playerState } =
    let
        colorAt : Int2 -> String
        colorAt (( _, y ) as gp) =
            if Set.member gp pp then
                listGetAtWithDefault "" (modBy 7 y) colors

            else
                "transparent"

        viewColumnAtX x =
            rangeN h
                |> List.map
                    (\y ->
                        let
                            gp =
                                ( x, y )
                        in
                        viewTile (colorAt gp) gp
                    )
                |> gCol
                    [ opacity
                        (if playerState == "playing" && x == cIdx then
                            0.5

                         else
                            1
                        )
                    ]
    in
    rangeN w
        |> List.map viewColumnAtX
        |> gRow
            [ style "flex-grow" "1"
            , noUserSelect
            , notifyPointerUp OnPointerUp
            ]


viewTile c gp =
    div
        [ bgc c
        , sOutline ("0.5px solid " ++ wLightGray)
        , sMinHeight "20px"
        , notifyPointerDown (PointerDownOnGP gp)
        , notifyPointerEnter (PointerEnteredGP gp)
        ]
        []
