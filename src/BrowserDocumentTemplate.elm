module BrowserDocumentTemplate exposing (main)

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


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


type Msg
    = NOP


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument _ =
    Document "App Title"
        [ basicStylesNode
        , text "BrowserDocumentTemplate"
        ]
