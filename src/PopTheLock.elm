module PopTheLock exposing (main)

import Curve
import Json.Decode as JD
import Random
import SubPath exposing (SubPath)
import Svg.Attributes as SA
import Svg.Events as SE
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:
    Clone: https://www.youtube.com/watch?v=eZAJTXmRtDc
    # STARTED ON: 26-01-22 10:10 AM
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
    PD
        { level : Int
        , pendingLocks : Int
        , phase : Phase
        , clock : Clock
        , seed : Seed
        }


type alias PDAngles =
    { pinAngle : Float, dotAngle : Float }


type alias PD a =
    { a
        | pinStartingAngle : Float
        , pinAngularDirection : AngularDirection
        , dotAngleOffset : Float
    }


pdAngles : Float -> PD a -> PDAngles
pdAngles elapsed pd =
    let
        pinAngle =
            pd.pinStartingAngle
                + angleInDirection pd.pinAngularDirection (pinAngularSpeed * elapsed)

        dotAngle =
            pd.pinStartingAngle
                + angleInDirection pd.pinAngularDirection pd.dotAngleOffset
    in
    { pinAngle = pinAngle, dotAngle = dotAngle }


pdIsPinOverDot : Float -> PD a -> Bool
pdIsPinOverDot elapsed pd =
    let
        failed =
            abs (pinAngularSpeed * elapsed - pd.dotAngleOffset) > errorMarginAngle
    in
    not failed



--pdRotate : Float -> PD a -> ( Bool, PD a )
--pdRotate dt pd =
--    let
--        elapsed =
--            pd.pinRotatedFor + dt
--
--        npd =
--            { pd | pinRotatedFor = elapsed }
--    in
--    ( not <| pdHasFailed npd, npd )


pdHasFailed : Float -> PD a -> Bool
pdHasFailed elapsed pd =
    pinAngularSpeed * elapsed > pd.dotAngleOffset + errorMarginAngle


type Phase
    = WaitingForUserInput
    | Rotating { pinRotatedFor : Float }
    | LevelCompleted PDAngles
    | LevelFailed PDAngles
    | NextLevelEntered


type alias Clock =
    Float


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
            Random.initialSeed 1
    in
    ( initWithSeed initialSeed
        |> updateOnUserInput
        |> update (OnClampedDelta (600 + 0))
        |> first
        |> updateOnUserInput
        |> Debug.log "Debug: "
    , Cmd.none
    )


initHelp : { level : Int, clock : Clock, initialSeed : Seed, phase : Phase } -> Model
initHelp { level, clock, initialSeed, phase } =
    let
        ( ( angularDirection, dotAngleOffset ), seed ) =
            Random.step
                (Random.pair
                    randomAngularDirection
                    randomDotAngleOffset
                )
                initialSeed
    in
    { level = level
    , pinStartingAngle = initialPinAngle
    , pinAngularDirection = angularDirection
    , dotAngleOffset = dotAngleOffset
    , pendingLocks = level
    , phase = phase
    , clock = clock
    , seed = seed
    }


initWithSeed : Seed -> Model
initWithSeed initialSeed =
    initHelp { level = 1, clock = 0, initialSeed = initialSeed, phase = WaitingForUserInput }


restartCurrentLevel : Model -> Model
restartCurrentLevel model =
    initHelp { level = model.level, clock = model.clock, initialSeed = model.seed, phase = WaitingForUserInput }


initNextLevel : Model -> Model
initNextLevel model =
    initHelp
        { level = model.level + 1
        , clock = model.clock
        , initialSeed = model.seed
        , phase = NextLevelEntered
        }


updateOnUserInput : Model -> Model
updateOnUserInput model =
    case model.phase of
        WaitingForUserInput ->
            { model | phase = Rotating { pinRotatedFor = 0 } }

        Rotating { pinRotatedFor } ->
            let
                pda =
                    pdAngles pinRotatedFor model
            in
            if pdIsPinOverDot pinRotatedFor model then
                if model.pendingLocks == 1 then
                    { model | pendingLocks = 0, phase = LevelCompleted pda }

                else
                    let
                        ( dotAngleOffset, seed ) =
                            Random.step randomDotAngleOffset model.seed
                    in
                    { level = model.level
                    , phase = Rotating { pinRotatedFor = 0 }
                    , clock = model.clock
                    , seed = seed
                    , pinStartingAngle = pda.pinAngle
                    , pinAngularDirection = oppositeAngularDirection model.pinAngularDirection
                    , dotAngleOffset = dotAngleOffset
                    , pendingLocks = model.pendingLocks - 1
                    }

            else
                { model | phase = LevelFailed pda }

        LevelCompleted _ ->
            model

        LevelFailed _ ->
            model

        NextLevelEntered ->
            model


step : Float -> Model -> Model
step dt model =
    case model.phase of
        WaitingForUserInput ->
            model

        Rotating { pinRotatedFor } ->
            let
                elapsed =
                    pinRotatedFor + dt

                phase =
                    case pdHasFailed elapsed model of
                        False ->
                            Rotating { pinRotatedFor = elapsed }

                        True ->
                            LevelFailed (pdAngles elapsed model)
            in
            { model | phase = phase }

        LevelCompleted _ ->
            model

        LevelFailed _ ->
            model

        NextLevelEntered ->
            model


type Msg
    = NOP
    | AnimationEnded String
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

        AnimationEnded name ->
            ( case ( model.phase, name ) of
                ( LevelCompleted _, "slideOutLeft" ) ->
                    initNextLevel model

                ( LevelFailed _, "headShake" ) ->
                    restartCurrentLevel model

                ( NextLevelEntered, "slideInRight" ) ->
                    { model | phase = WaitingForUserInput }

                _ ->
                    model
            , Cmd.none
            )

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
        , styleNode """
            @keyframes popLockHandle {
                0% {
                    transform: translate(0,0);
                }

                25% {
                    transform: translate(0, 2%);
                }

                50%, 100% {
                    transform: translate(0, -10%);
                }


            }
        """
        , toViewModel model |> view

        --, div [ positionFixed, bgc <| blackA 0.3 ] [ text <| Debug.toString model.phase ]
        ]


type alias ViewModel =
    { bgColor : String
    , level : Int
    , pendingLocks : Int
    , pinAngle : Float
    , dotAngle : Maybe Float
    , classes : List String
    , style : String
    , lockHandleClasses : ( List String, String )
    }


view : ViewModel -> Svg Msg
view vm =
    basicSvg
        [ viewBoxC 300 (300 * 1.5)
        , sMaxHeight "100vh"
        , bgc vm.bgColor
        ]
        [ viewLevelNum vm.level
        , group [ transforms [ translateF2 ( 0, 50 ) ] ]
            [ group
                [ classNames vm.classes
                , SA.style vm.style
                , SE.on "animationend" (JD.map AnimationEnded (JD.field "animationName" JD.string))
                ]
                [ viewLock vm.lockHandleClasses vm.bgColor
                , maybeView viewDot vm.dotAngle
                , viewPin vm.pinAngle
                , viewPendingLocks vm.pendingLocks
                ]
            ]
        ]


toViewModel : Model -> ViewModel
toViewModel model =
    let
        pda =
            case model.phase of
                WaitingForUserInput ->
                    pdAngles 0 model

                Rotating { pinRotatedFor } ->
                    pdAngles pinRotatedFor model

                LevelCompleted pda_ ->
                    pda_

                LevelFailed pda_ ->
                    pda_

                NextLevelEntered ->
                    pdAngles 0 model

        vm : ViewModel
        vm =
            { bgColor = getBGColor model.phase
            , level = model.level
            , pendingLocks = model.pendingLocks
            , pinAngle = pda.pinAngle
            , dotAngle = Just pda.dotAngle
            , classes = []
            , style = ""
            , lockHandleClasses = ( [], "" )
            }
    in
    case model.phase of
        WaitingForUserInput ->
            vm

        Rotating _ ->
            vm

        LevelCompleted _ ->
            { vm
                | lockHandleClasses =
                    ( []
                    , "animation: popLockHandle; animation-fill-mode: both; animation-duration: 1000ms"
                    )
                , dotAngle = Nothing
                , classes = [ cnAnimated, cnSlideOutLeft ]
                , style = "animation-delay: 1000ms; animation-duration: 500ms"
            }

        LevelFailed _ ->
            { vm | classes = [ cnAnimated, cnHeadShake ] }

        NextLevelEntered ->
            { vm
                | classes = [ cnAnimated, cnSlideInRight ]
                , style = "animation-duration: 200ms"
            }


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


viewLock : ( List String, String ) -> String -> Svg Msg
viewLock ( lockHandleClasses, lockHandleStyle ) bg =
    [ group [ classNames lockHandleClasses, SA.style lockHandleStyle ]
        [ SubPath.element
            (lockHandleSubPath lockRadius)
            [ transforms
                [ translateF2 ( 0, -lockRadius )
                , scaleY 1.2
                ]
            , stroke <| blackA 0.6
            , strokeCapRound
            ]
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
    viewDotAnimated__ { angle = angle, scale = 1 }


viewDotAnimated__ : { scale : Float, angle : Float } -> Svg Msg
viewDotAnimated__ { scale, angle } =
    let
        r =
            lockRadius

        theta =
            angle

        dotCenterF2 =
            fromPolar ( r, theta )
    in
    circle dotRadius
        [ fill wYellow
        , transforms
            [ translateF2 dotCenterF2
            , scaleF scale
            ]
        ]


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
