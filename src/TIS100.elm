module TIS100 exposing (main)

import Html exposing (node)
import Html.Attributes exposing (attribute)
import TIS100.PuzzlePage as PuzzlePage
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
    ( { page = PuzzlePage.sampleModel }, Cmd.none )


type Msg
    = PuzzlePageMsg PuzzlePage.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    PuzzlePage.subscriptions model.page
        |> Sub.map PuzzlePageMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PuzzlePageMsg sm ->
            ( { model | page = PuzzlePage.update sm model.page }, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, node "SCRIPT" [ attribute "src" "https://livejs.com/live.js" ] []
        , PuzzlePage.view model.page |> Html.map PuzzlePageMsg
        ]


view : PuzzlePage.Model -> Html Msg
view sim =
    div []
        []
