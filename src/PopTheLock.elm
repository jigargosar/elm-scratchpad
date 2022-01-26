module PopTheLock exposing (main)

import Random
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT
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
    , seed : Seed
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


randomInitialPhase : Generator Phase
randomInitialPhase =
    randomAngularDirection
        |> Random.andThen
            (\angularDirection ->
                randomDotAngle initialPinAngle angularDirection
                    |> Random.map
                        (\dotAngle ->
                            WaitingForUserInput
                                { dotAngle = dotAngle
                                , pinAngularDirection = angularDirection
                                }
                        )
            )


randomDotAngle : Float -> AngularDirection -> Generator Float
randomDotAngle pinAngle angularDirection =
    randomDotAngleOffset
        |> (case angularDirection of
                ClockWise ->
                    Random.map negate

                CounterClockWise ->
                    identity
           )
        |> Random.map (add pinAngle)


randomDotAngleOffset =
    Random.float minDotAngleOffset maxDotAngleOffset


maxDotAngleOffset =
    degrees (90 + 45)


minDotAngleOffset =
    degrees 20


randomAngularDirection : Generator AngularDirection
randomAngularDirection =
    Random.uniform ClockWise [ CounterClockWise ]


type AngularDirection
    = ClockWise
    | CounterClockWise


initialPinAngle =
    degrees -90


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialSeed =
            Random.initialSeed 2

        ( phase, seed ) =
            Random.step randomInitialPhase initialSeed
    in
    ( { level = 1
      , phase = phase
      , seed = seed
      }
        |> updateOnUserInput
    , Cmd.none
    )


updateOnUserInput : Model -> Model
updateOnUserInput model =
    case model.phase of
        WaitingForUserInput _ ->
            model

        Rotating _ ->
            model

        LevelFailed _ ->
            model

        LevelComplete _ ->
            model


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
        , bgc wBlue
        ]
        [ viewLevelNum model.level
        , case model.phase of
            WaitingForUserInput { dotAngle } ->
                group []
                    [ viewLock
                    , viewDot dotAngle
                    , viewPin initialPinAngle
                    , viewPendingLocks model.level
                    ]

            Rotating _ ->
                noView

            LevelFailed _ ->
                noView

            LevelComplete _ ->
                noView
        ]


lockRadius =
    90


lockThickness =
    30


pinRadius =
    (lockThickness / 2) * 0.7


dotRadius =
    (lockThickness / 2) * 0.6


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
        , strokeW 8
        , TA.strokeLinecap TT.StrokeLinecapRound
        , transforms [ rotateF angle, translateF2 ( lockRadius, 0 ) ]
        ]


viewPendingLocks : Int -> Svg Msg
viewPendingLocks num =
    let
        numStr =
            fromInt num
    in
    words numStr
        [ fontSize "80px"
        , ffMonospace
        , fill <| whiteA 0.8
        ]


viewLevelNum : Int -> Svg Msg
viewLevelNum level =
    let
        levelStr =
            fromInt level

        txt =
            "LEVEL: " ++ levelStr
    in
    words txt
        [ wordsAlignYTop
        , wordsAlignXLeft
        , fontSize "30px"
        , ffMonospace
        , fill <| whiteA 0.8
        , transforms [ translateT ( "-50%", "-50%" ), translateF2 ( 10, 10 ) ]
        ]
