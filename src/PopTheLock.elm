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
        , view = viewDoc
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


viewDoc : Model -> Document Msg
viewDoc model =
    Document "App Title"
        [ basicStylesNode
        , view model
        ]


view : Model -> Html Msg
view model =
    basicSvg [ viewBoxC 300 600, bgc wPurple ]
        [ viewLevelNum model.level
        , case model.phase of
            WaitingForUserInput { dotAngle } ->
                group []
                    [ viewLock
                    , viewDot dotAngle
                    , viewPin initialPinAngle
                    ]

            Rotating _ ->
                noView

            LevelFailed _ ->
                noView

            LevelComplete _ ->
                noView
        ]


viewLock : Svg Msg
viewLock =
    circle 75 [ strokeW 30, stroke <| blackA 0.8 ]


viewLevelNum : Int -> Svg Msg
viewLevelNum level =
    let
        levelStr =
            fromInt level

        txt =
            "LEVEL:" ++ levelStr
    in
    words txt
        [ wordsAlignYTop
        , wordsAlignXLeft
        , fontSize "30px"
        , fill <| whiteA 0.8
        , transforms [ translateF2 ( 20, 20 ) ]
        ]
