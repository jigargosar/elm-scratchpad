module PopTheLock exposing (main)

import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:
    Clone: https://www.youtube.com/watch?v=eZAJTXmRtDc
-}


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { level : Int
    , phase : Phase
    }


type Phase
    = WaitingForUserInput


init : () -> ( Model, Cmd Msg )
init () =
    ( { level = 1
      , phase = WaitingForUserInput
      }
    , Cmd.none
    )


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


view : Model -> Document Msg
view _ =
    Document "App Title"
        [ basicStylesNode
        ]
