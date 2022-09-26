module TIS100 exposing (main)

import Browser.Dom
import Html exposing (node)
import Html.Attributes exposing (attribute)
import TIS100.Effect exposing (Effect(..))
import TIS100.PuzzlePage as PuzzlePage
import Task
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


main =
    browserDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type Page
    = PuzzlePage PuzzlePage.Model
    | SegmentListPage


type alias Model =
    { page : Page }


init : () -> ( Model, Cmd Msg )
init () =
    ( { page =
            PuzzlePage PuzzlePage.signalComparatorModel
                |> always SegmentListPage
      }
    , Cmd.none
    )


type Msg
    = PuzzlePageMsg PuzzlePage.Msg
    | OnFocus (Result Browser.Dom.Error ())


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PuzzlePage ppm ->
            PuzzlePage.subscriptions ppm
                |> Sub.map PuzzlePageMsg

        SegmentListPage ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PuzzlePageMsg sm ->
            case model.page of
                PuzzlePage ppm ->
                    let
                        ( page, effect ) =
                            PuzzlePage.update sm ppm
                    in
                    { model | page = PuzzlePage page }
                        |> runEffect effect

                SegmentListPage ->
                    ( model, Cmd.none )

        OnFocus (Ok ()) ->
            ( model, Cmd.none )

        OnFocus (Err err) ->
            let
                _ =
                    Debug.log "Focus Error: " err
            in
            ( model, Cmd.none )


runEffect : Effect -> Model -> ( Model, Cmd Msg )
runEffect effect model =
    case effect of
        Focus hid ->
            ( model, Browser.Dom.focus hid |> Task.attempt OnFocus )

        None ->
            ( model, Cmd.none )

        ReturnToSegmentList ->
            ( { model | page = SegmentListPage }, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, node "SCRIPT" [ attribute "src" "https://livejs.com/live.js" ] []
        , case model.page of
            PuzzlePage page ->
                PuzzlePage.view page |> Html.map PuzzlePageMsg

            SegmentListPage ->
                div [ displayGrid, placeContentCenter ]
                    [ button [] [ text "go to puzzle" ]
                    ]
        ]
