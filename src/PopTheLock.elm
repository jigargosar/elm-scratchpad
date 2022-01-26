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
    = WaitingForUserInput { dotAngle : Float, pinAngularDirection : AngularDirection }
    | Rotating
        { pinAngle : Float
        , dotAngle : Float
        , pinAngularDirection : AngularDirection
        , locksPopped : Int
        }
    | LevelFailed { pinAngle : Float, dotAngle : Float, locksPopped : Int }
    | LevelComplete { pinAngle : Float }


type AngularDirection
    = ClockWise
    | CounterClockWise


init : () -> ( Model, Cmd Msg )
init () =
    ( { level = 1
      , phase = WaitingForUserInput { dotAngle = 0, pinAngularDirection = CounterClockWise }
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
