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


initialPinAngle =
    degrees -90


pinAngularSpeed =
    degreesPerSecond 30


degreesPerSecond d =
    degrees d / 1000


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
    | LevelFailed { pinAngle : Float, dotAngle : Float, pendingLocks : Int }
    | LevelComplete { pinAngle : Float }


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


angularVelocity : Float -> AngularDirection -> Float
angularVelocity angularSpeed angularDirection =
    angularSpeed * angularDirectionToSign angularDirection


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

        Rotating _ ->
            model

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
                    pinAngularSpeed * elapsed > rec.dotAngleOffset + degrees 10
            in
            if failed then
                { model
                    | phase =
                        LevelFailed
                            { pinAngle = rec.pinStartingAngle
                            , dotAngle = rec.dotAngleOffset
                            , pendingLocks = rec.pendingLocks
                            }
                }

            else
                { model | phase = Rotating { rec | elapsed = elapsed } }

        LevelFailed _ ->
            model

        LevelComplete _ ->
            model


type Msg
    = NOP
    | OnClampedDelta Float


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameClampedDelta OnClampedDelta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnClampedDelta dt ->
            ( step dt model, Cmd.none )


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
            WaitingForUserInput { dotAngleOffset, pinAngularDirection } ->
                let
                    pinAngle =
                        initialPinAngle

                    dotAngle =
                        pinAngle + angleInDirection pinAngularDirection dotAngleOffset

                    pendingLocks =
                        model.level
                in
                group []
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
                        model.level
                in
                group []
                    [ viewLock
                    , viewDot dotAngle
                    , viewPin pinAngle
                    , viewPendingLocks pendingLocks
                    ]

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
