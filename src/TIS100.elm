module TIS100 exposing (main)

import Browser.Dom
import Html exposing (node)
import Html.Attributes exposing (attribute)
import TIS100.Effect as Eff exposing (Effect(..), withEff, withoutEff)
import TIS100.PuzzlePage as PuzzlePage
import Task
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


main =
    let
        init_ flags =
            init flags |> runEffect

        update_ msg model =
            update msg model |> runEffect
    in
    browserDocument
        { init = init_
        , subscriptions = subscriptions
        , update = update_
        , view = viewDocument
        }


type Page
    = PuzzlePage PuzzlePage.Model
    | SegmentListPage


type alias Model =
    { page : Page }


init : () -> ( Model, Effect )
init () =
    { page =
        PuzzlePage PuzzlePage.signalComparatorModel
            |> always SegmentListPage
    }
        |> withEff Eff.autoFocus


type Msg
    = PuzzlePageMsg PuzzlePage.Msg
    | GotoPuzzle
    | OnFocus (Result Browser.Dom.Error ())


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PuzzlePage ppm ->
            PuzzlePage.subscriptions ppm
                |> Sub.map PuzzlePageMsg

        SegmentListPage ->
            Sub.none


update : Msg -> Model -> ( Model, Effect )
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
                        |> withEff effect

                SegmentListPage ->
                    model |> withoutEff

        GotoPuzzle ->
            { model | page = PuzzlePage PuzzlePage.signalComparatorModel }
                |> withoutEff

        OnFocus (Ok ()) ->
            model |> withoutEff

        OnFocus (Err err) ->
            let
                _ =
                    Debug.log "Focus Error: " err
            in
            model |> withoutEff


runEffect : ( Model, Effect ) -> ( Model, Cmd Msg )
runEffect ( model, effect ) =
    case effect of
        Focus hid ->
            ( model, Browser.Dom.focus hid |> Task.attempt OnFocus )

        None ->
            ( model, Cmd.none )

        ReturnToSegmentList ->
            { model | page = SegmentListPage }
                |> withEff Eff.autoFocus
                |> runEffect


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
                    [ button
                        [ Eff.attrAutoFocusId
                        , notifyClick GotoPuzzle
                        ]
                        [ text "go to puzzle" ]
                    ]
        ]
