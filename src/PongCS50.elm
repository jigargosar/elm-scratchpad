module PongCS50 exposing (main)

import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:
    * Clone: https://www.youtube.com/watch?v=GfwpRU0cT10&list=PLhQjrBD2T383Vx9-4vJYFsJbvZ_D17Qzh&index=2
    # Why?
    * create a video/text tutorial series.
    * code kata
    * make a complete game

    # Next up
    * rectangle update

    # Later
    * retro font update




-}


main =
    bDocument
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
    Document "PongCS50"
        [ basicStylesNode
        ]
