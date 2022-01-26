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


initialPinAngle =
    degrees -90


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
    basicSvg
        [ viewBoxC 300 (300 * 1.5)
        , sMaxHeight "100vh"
        , bgc wPurple
        ]
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


lockRadius =
    75


lockThickness =
    30


pinRadius =
    (lockThickness / 2) * 0.9


dotRadius =
    (lockThickness / 2) * 0.8


viewLock : Svg Msg
viewLock =
    circle lockRadius [ strokeW lockThickness, stroke <| blackA 0.8 ]


viewDot : Float -> Svg Msg
viewDot angle =
    let
        r =
            lockRadius

        theta =
            angle

        dotCenterF2 =
            fromPolar ( r, theta )
    in
    circle dotRadius [ fill wYellow, transforms [ translateF2 dotCenterF2 ] ]


viewPin : Float -> Svg Msg
viewPin angle =
    polyline [ ( -pinRadius, 0 ), ( pinRadius, 0 ) ]
        [ stroke wPink
        , strokeW 10
        , transforms [ translateF2 ( lockRadius, 0 ), rotateF angle ]
        ]


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
