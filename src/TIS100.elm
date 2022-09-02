module TIS100 exposing (main)

import Html
import TIS100.PuzzleScreen as PuzzlePage
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
    = SimMsg PuzzlePage.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    PuzzlePage.subscriptions model.page
        |> Sub.map SimMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SimMsg sm ->
            ( { model | page = PuzzlePage.update sm model.page }, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, text "BrowserDocumentTemplate"
        , view model.page
        ]


view : PuzzlePage.Model -> Html Msg
view sim =
    div []
        [ div [] [ text <| Debug.toString sim ]
            |> always noView
        , PuzzlePage.view sim |> Html.map SimMsg
        ]
