module TIS100 exposing (main)

import Html
import TIS100.Sim as Sim
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
    { sim : Sim.Model }


init : () -> ( Model, Cmd Msg )
init () =
    ( { sim = Sim.sampleModel }, Cmd.none )


type Msg
    = SimMsg Sim.Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SimMsg sm ->
            ( { model | sim = Sim.update sm model.sim }, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, text "BrowserDocumentTemplate"
        , view model.sim
        ]


view : Sim.Model -> Html Msg
view sim =
    div []
        [ div [] [ text <| Debug.toString sim ]
            |> always noView
        , Sim.view sim |> Html.map SimMsg
        ]
