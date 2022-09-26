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
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    { page : PuzzlePage.Model }


init : () -> ( Model, Cmd Msg )
init () =
    ( { page = PuzzlePage.signalComparatorModel }, Cmd.none )


type Msg
    = PuzzlePageMsg PuzzlePage.Msg
    | OnFocus (Result Browser.Dom.Error ())


subscriptions : Model -> Sub Msg
subscriptions model =
    PuzzlePage.subscriptions model.page
        |> Sub.map PuzzlePageMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PuzzlePageMsg sm ->
            let
                ( page, effect ) =
                    PuzzlePage.update sm model.page
            in
            ( { model | page = page }, runEffect effect )

        OnFocus (Ok ()) ->
            ( model, Cmd.none )

        OnFocus (Err err) ->
            let
                _ =
                    Debug.log "Focus Error: " err
            in
            ( model, Cmd.none )


runEffect : Effect -> Cmd Msg
runEffect effect =
    case effect of
        Focus hid ->
            Browser.Dom.focus hid |> Task.attempt OnFocus

        None ->
            Cmd.none


viewDocument : Model -> Document Msg
viewDocument model =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, node "SCRIPT" [ attribute "src" "https://livejs.com/live.js" ] []
        , PuzzlePage.view model.page |> Html.map PuzzlePageMsg
        ]
