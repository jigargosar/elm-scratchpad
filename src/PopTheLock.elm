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


maxDotAngleOffset =
    degrees (90 + 45)


minDotAngleOffset =
    degrees 20


errorMarginAngle =
    degrees 8


initialPinAngle =
    degrees -90


pinAngularSpeed =
    degreesPerSecond 60


degreesPerSecond d =
    degrees d / 1000


maxLevelFailedTransitionDuration =
    --3000
    1000


maxLevelCompleteTransitionOutDuration =
    1500


type alias Model =
    { level : Int
    , phase : Phase
    , seed : Seed
    }


type Phase
    = WaitingForUserInput { dotAngleOffset : Float, pinAngularDirection : AngularDirection }
    | Rotating
        { pinStartingAngle : Float
        , elapsed : Float
        , dotAngleOffset : Float
        , pinAngularDirection : AngularDirection
        , pendingLocks : Int
        }
    | LevelFailed { elapsed : Float, pinAngle : Float, dotAngle : Float, pendingLocks : Int }
    | LevelComplete { elapsed : Float, pinAngle : Float }


randomInitialPhase : Generator Phase
randomInitialPhase =
    Random.map2
        (\angularDirection dotAngleOffset ->
            WaitingForUserInput
                { dotAngleOffset = dotAngleOffset
                , pinAngularDirection = angularDirection
                }
        )
        randomAngularDirection
        randomDotAngleOffset


randomDotAngleOffset =
    Random.float minDotAngleOffset maxDotAngleOffset


randomAngularDirection : Generator AngularDirection
randomAngularDirection =
    Random.uniform ClockWise [ CounterClockWise ]


type AngularDirection
    = ClockWise
    | CounterClockWise


oppositeAngularDirection : AngularDirection -> AngularDirection
oppositeAngularDirection angularDirection =
    case angularDirection of
        ClockWise ->
            CounterClockWise

        CounterClockWise ->
            ClockWise


angleInDirection : AngularDirection -> Float -> Float
angleInDirection angularDirection angle =
    angularDirectionToSign angularDirection * angle


angularDirectionToSign : AngularDirection -> Float
angularDirectionToSign angularDirection =
    case angularDirection of
        ClockWise ->
            1

        CounterClockWise ->
            -1


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialSeed =
            Random.initialSeed 2
    in
    ( initLevelWithSeed 1 initialSeed
        |> updateOnUserInput
        |> step (4500 + 0)
        |> updateOnUserInput
        |> Debug.log "Debug: "
    , Cmd.none
    )


initLevelWithSeed : Int -> Seed -> Model
initLevelWithSeed level initialSeed =
    let
        ( phase, seed ) =
            Random.step randomInitialPhase initialSeed
    in
    { level = level
    , phase = phase
    , seed = seed
    }


updateOnUserInput : Model -> Model
updateOnUserInput model =
    case model.phase of
        WaitingForUserInput { dotAngleOffset, pinAngularDirection } ->
            { model
                | phase =
                    Rotating
                        { pinStartingAngle = initialPinAngle
                        , elapsed = 0
                        , dotAngleOffset = dotAngleOffset
                        , pinAngularDirection = pinAngularDirection
                        , pendingLocks = model.level
                        }
            }

        Rotating rec ->
            let
                failed =
                    abs (pinAngularSpeed * rec.elapsed - rec.dotAngleOffset) > errorMarginAngle
            in
            let
                pinAngle =
                    rec.pinStartingAngle
                        + angleInDirection rec.pinAngularDirection (pinAngularSpeed * rec.elapsed)
            in
            if failed then
                let
                    dotAngle =
                        rec.pinStartingAngle
                            + angleInDirection rec.pinAngularDirection rec.dotAngleOffset

                    pendingLocks =
                        rec.pendingLocks
                in
                { model
                    | phase =
                        LevelFailed
                            { elapsed = 0
                            , pinAngle = pinAngle
                            , dotAngle = dotAngle
                            , pendingLocks = pendingLocks
                            }
                }

            else if rec.pendingLocks <= 1 then
                { model | phase = LevelComplete { elapsed = 0, pinAngle = pinAngle } }

            else
                let
                    ( dotAngleOffset, seed ) =
                        Random.step randomDotAngleOffset model.seed
                in
                { model
                    | seed = seed
                    , phase =
                        Rotating
                            { pinStartingAngle = pinAngle
                            , elapsed = 0
                            , dotAngleOffset = dotAngleOffset
                            , pinAngularDirection = oppositeAngularDirection rec.pinAngularDirection
                            , pendingLocks = rec.pendingLocks - 1
                            }
                }

        LevelFailed _ ->
            model

        LevelComplete _ ->
            model


step : Float -> Model -> Model
step dt model =
    case model.phase of
        WaitingForUserInput _ ->
            model

        Rotating rec ->
            let
                elapsed =
                    rec.elapsed + dt

                failed =
                    pinAngularSpeed * elapsed > rec.dotAngleOffset + errorMarginAngle
            in
            if failed then
                let
                    pinAngle =
                        rec.pinStartingAngle
                            + angleInDirection rec.pinAngularDirection (pinAngularSpeed * elapsed)

                    dotAngle =
                        rec.pinStartingAngle
                            + angleInDirection rec.pinAngularDirection rec.dotAngleOffset

                    pendingLocks =
                        rec.pendingLocks
                in
                { model
                    | phase =
                        LevelFailed
                            { elapsed = 0
                            , pinAngle = pinAngle
                            , dotAngle = dotAngle
                            , pendingLocks = pendingLocks
                            }
                }

            else
                { model | phase = Rotating { rec | elapsed = elapsed } }

        LevelFailed rec ->
            let
                maxValue =
                    maxLevelFailedTransitionDuration

                elapsed =
                    rec.elapsed + dt |> atMost maxValue
            in
            if elapsed == maxValue then
                initLevelWithSeed model.level model.seed

            else
                { model | phase = LevelFailed { rec | elapsed = elapsed } }

        LevelComplete rec ->
            let
                maxValue =
                    maxLevelCompleteTransitionOutDuration

                elapsed =
                    rec.elapsed + dt |> atMost maxValue
            in
            if elapsed == maxValue then
                initLevelWithSeed (model.level + 1) model.seed

            else
                { model | phase = LevelComplete { rec | elapsed = elapsed } }


type Msg
    = NOP
    | OnClampedDelta Float
    | OnKeyDown KeyEvent


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ onAnimationFrameClampedDelta OnClampedDelta
    , onBrowserKeyDown OnKeyDown
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnClampedDelta dt ->
            ( step dt model, Cmd.none )

        OnKeyDown keyEvent ->
            ( if keyEvent.key == " " && not keyEvent.repeat then
                updateOnUserInput model

              else
                model
            , Cmd.none
            )


viewDoc : Model -> Document Msg
viewDoc model =
    Document "App Title"
        [ basicStylesNode
        , animateCssNode
        , view model

        --, div [ positionFixed, bgc <| blackA 0.3 ] [ text <| Debug.toString model.phase ]
        ]


view : Model -> Html Msg
view model =
    basicSvg
        [ viewBoxC 300 (300 * 1.5)
        , sMaxHeight "100vh"
        , bgc (getBGColor model.phase)
        ]
        [ viewLevelNum model.level
        , case model.phase of
            WaitingForUserInput { dotAngleOffset, pinAngularDirection } ->
                let
                    pinAngle =
                        initialPinAngle

                    dotAngle =
                        pinAngle + angleInDirection pinAngularDirection dotAngleOffset

                    pendingLocks =
                        model.level
                in
                group
                    [ classNames
                        (if model.level /= 1 then
                            [ cnAnimated, cnSlideInRight, cnFaster ]

                         else
                            []
                        )
                    ]
                    [ viewLock
                    , viewDot dotAngle
                    , viewPin pinAngle
                    , viewPendingLocks pendingLocks
                    ]

            Rotating rec ->
                let
                    pinAngle =
                        rec.pinStartingAngle
                            + angleInDirection rec.pinAngularDirection (pinAngularSpeed * rec.elapsed)

                    dotAngle =
                        rec.pinStartingAngle
                            + angleInDirection rec.pinAngularDirection rec.dotAngleOffset

                    pendingLocks =
                        rec.pendingLocks
                in
                group []
                    [ viewLock
                    , viewDot dotAngle
                    , viewPin pinAngle
                    , viewPendingLocks pendingLocks
                    ]

            LevelFailed rec ->
                let
                    pinAngle =
                        rec.pinAngle

                    --dotAngle = rec.dotAngle
                    pendingLocks =
                        rec.pendingLocks
                in
                group
                    [ classNames [ cnAnimated, cnHeadShake ]
                    ]
                    [ viewLock

                    --, viewDot dotAngle
                    , viewPin pinAngle
                    , viewPendingLocks pendingLocks
                    ]

            LevelComplete rec ->
                let
                    pinAngle =
                        rec.pinAngle

                    --dotAngle = rec.dotAngle
                    pendingLocks =
                        0
                in
                group
                    [ classNames [ cnAnimated, cnSlideOutLeft, cnFaster, "animate__delay-1s" ]
                    ]
                    [ viewLock

                    --, viewDot dotAngle
                    , viewPin pinAngle
                    , viewPendingLocks pendingLocks
                    ]
        ]


lockRadius =
    90


lockThickness =
    30


pinRadius =
    (lockThickness / 2) * 0.7


dotRadius =
    (lockThickness / 2) * 0.6


getBGColor : Phase -> String
getBGColor phase =
    let
        isFail =
            case phase of
                LevelFailed _ ->
                    True

                _ ->
                    False
    in
    if isFail then
        "tomato"

    else
        wBlue


viewLock : Svg Msg
viewLock =
    [ polyline
        [ ( -lockRadius / 2, 0 )
        , ( -lockRadius / 2, -lockRadius )
        , ( lockRadius / 2, -lockRadius )
        , ( lockRadius / 2, 0 )
        ]
        [ transforms [ translateF2 ( 0, -lockRadius * 0.9 ) ]
        , strokeW lockThickness
        , stroke <| blackA 0.8
        ]
    , circle lockRadius [ strokeW lockThickness, stroke <| blackA 0.95 ]
    ]
        |> group []


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
