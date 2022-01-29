module PopTheLock exposing (main)

import Curve
import Ease
import Random
import SubPath exposing (SubPath)
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


type alias Model =
    { level : Int
    , phase : Phase
    , clock : Clock
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
    | LevelFailed { animation : Animation, pinAngle : Float, dotAngle : Float, pendingLocks : Int }
    | LevelComplete { animation : Animation, pinAngle : Float }


initLevelComplete : { pinAngle : Float, clock : Clock } -> Phase
initLevelComplete { pinAngle, clock } =
    LevelComplete
        { animation = startAnimation ( 500, [ 500, 500 ] ) clock
        , pinAngle = 0
        }


initLevelFailed :
    { clock : Clock
    , pinAngle : Float
    , dotAngle : Float
    , pendingLocks : Int
    }
    -> Phase
initLevelFailed { clock, pinAngle, dotAngle, pendingLocks } =
    LevelFailed
        { animation = startAnimation ( 1000, [] ) clock
        , pinAngle = pinAngle
        , dotAngle = dotAngle
        , pendingLocks = pendingLocks
        }


type alias Clock =
    Float


type alias Animation =
    { durations : NEL Float, startClock : Clock }


startAnimation : NEL Float -> Clock -> Animation
startAnimation durations clock =
    { durations = durations, startClock = clock }


animationIsDone : Animation -> Clock -> Bool
animationIsDone { startClock, durations } nowClock =
    nowClock - startClock >= first durations + List.sum (second durations)


animationValue : Animation -> Clock -> ( Int, Float )
animationValue { durations, startClock } nowClock =
    let
        elapsed =
            nowClock - startClock
    in
    if elapsed <= 0 then
        ( 0, 0 )

    else
        animationValueHelp elapsed 0 0 durations


animationValueHelp : Float -> Int -> Float -> NEL Float -> ( Int, Float )
animationValueHelp elapsed i start ( dur, ds ) =
    let
        end =
            start + dur
    in
    if elapsed < end then
        ( i, norm start end elapsed )

    else
        case uncons ds of
            Nothing ->
                ( i, 1 )

            Just nDurations ->
                animationValueHelp elapsed (i + 1) end nDurations


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
        |> update (OnClampedDelta (2200 + 0))
        |> first
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
    , clock = 0
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
                        initLevelFailed
                            { clock = model.clock
                            , pinAngle = pinAngle
                            , dotAngle = dotAngle
                            , pendingLocks = pendingLocks
                            }
                }

            else if rec.pendingLocks <= 1 then
                { model | phase = initLevelComplete { pinAngle = pinAngle, clock = model.clock } }

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
                        initLevelFailed
                            { clock = model.clock
                            , pinAngle = pinAngle
                            , dotAngle = dotAngle
                            , pendingLocks = pendingLocks
                            }
                }

            else
                { model | phase = Rotating { rec | elapsed = elapsed } }

        LevelFailed rec ->
            if animationIsDone rec.animation model.clock then
                initLevelWithSeed model.level model.seed

            else
                model

        LevelComplete rec ->
            if animationIsDone rec.animation model.clock then
                initLevelWithSeed (model.level + 1) model.seed

            else
                model


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
            ( step dt { model | clock = model.clock + dt }, Cmd.none )

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
    let
        bgColor =
            getBGColor model.phase
    in
    basicSvg
        [ viewBoxC 300 (300 * 1.5)
        , sMaxHeight "100vh"
        , bgc bgColor
        ]
        [ viewLevelNum model.level
        , group [ transforms [ translateF2 ( 0, 50 ) ] ]
            [ case model.phase of
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
                        [ viewLock bgColor
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
                        [ viewLock bgColor
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
                        [ viewLock bgColor

                        --, viewDot dotAngle
                        , viewPin pinAngle
                        , viewPendingLocks pendingLocks
                        ]

                LevelComplete rec ->
                    let
                        pinAngle =
                            rec.pinAngle

                        pendingLocks =
                            0

                        ( partIdx, n ) =
                            animationValue rec.animation model.clock

                        dx =
                            if partIdx == 2 then
                                n |> Ease.inBack |> mul -300

                            else
                                0

                        lockHandleDY =
                            (if partIdx == 0 then
                                n

                             else
                                1
                            )
                                |> Ease.inBack
                                |> mul -50
                    in
                    group [ transforms [ translateF2 ( dx, 0 ) ] ]
                        [ viewLockAnimated { lockHandleDY = lockHandleDY } bgColor
                        , viewPin pinAngle
                        , viewPendingLocks pendingLocks
                        ]
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


viewLock : String -> Svg Msg
viewLock =
    viewLockAnimated { lockHandleDY = 0 }


viewLockAnimated : { lockHandleDY : Float } -> String -> Svg Msg
viewLockAnimated { lockHandleDY } bg =
    [ SubPath.element
        (lockHandleSubPath lockRadius)
        [ transforms
            [ translateF2 ( 0, -lockRadius + lockHandleDY )
            , scaleY 1.2
            ]
        , stroke <| blackA 0.6
        , strokeCapRound
        ]
    , circle lockRadius [ stroke bg ]
    , circle lockRadius [ stroke (blackA 0.9) ]
    ]
        |> group [ strokeW lockThickness ]


lockHandleSubPath : Float -> SubPath
lockHandleSubPath r =
    Curve.basis
        [ ( -r / 2, 0 )
        , ( -r / 2, -r )
        , ( r / 2, -r )
        , ( r / 2, 0 )
        ]


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
        , strokeCapRound
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
