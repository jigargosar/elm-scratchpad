module TIS100 exposing (main)

import Html
import TIS100.PuzzleScreen as PuzzleScreen
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
    { screen : PuzzleScreen.Model }


init : () -> ( Model, Cmd Msg )
init () =
    ( { screen = PuzzleScreen.sampleModel }, Cmd.none )


type Msg
    = SimMsg PuzzleScreen.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    PuzzleScreen.subscriptions model.screen
        |> Sub.map SimMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SimMsg sm ->
            ( { model | screen = PuzzleScreen.update sm model.screen }, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, text "BrowserDocumentTemplate"
        , view model.screen
        ]


view : PuzzleScreen.Model -> Html Msg
view sim =
    div []
        [ div [] [ text <| Debug.toString sim ]
            |> always noView
        , PuzzleScreen.view sim |> Html.map SimMsg
        ]
