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
        , seed : Seed
        }


type alias PDAngles =
    { pinAngle : Float, dotAngle : Float }


type alias PD a =
    { a
        | pinRotatedFor : Float
        , pinStartingAngle : Float
        , pinAngularDirection : AngularDirection
        , dotAngleOffset : Float
    }


pinAngle : PD a -> Float
pinAngle pd =
    pd.pinStartingAngle
        + angleInDirection pd.pinAngularDirection (pinAngularSpeed * pd.pinRotatedFor)


dotAngle : PD a -> Float
dotAngle pd =
    pd.pinStartingAngle
        + angleInDirection pd.pinAngularDirection pd.dotAngleOffset


isPinOverDot : PD a -> Bool
isPinOverDot pd =
    abs (pinAngularSpeed * pd.pinRotatedFor - pd.dotAngleOffset) <= errorMarginAngle


hasPinGoneBeyondDot : PD a -> Bool
hasPinGoneBeyondDot pd =
    pinAngularSpeed * pd.pinRotatedFor > pd.dotAngleOffset + errorMarginAngle


type Phase
    = WaitingForUserInput
    | Rotating
    | LevelCompleted
    | LevelFailed
    | NextLevelEntered


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
    ( initHelp
        { level = 1
        , initialSeed = initialSeed
        , phase = WaitingForUserInput
        }
        |> updateOnUserInput
        |> update (OnClampedDelta (600 + 0))
        |> first
        |> updateOnUserInput
        |> Debug.log "Debug: "
    , Cmd.none
    )


initHelp : { level : Int, initialSeed : Seed, phase : Phase } -> Model
initHelp { level, initialSeed, phase } =
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
    , pinRotatedFor = 0
    , pinStartingAngle = initialPinAngle
    , pinAngularDirection = angularDirection
    , dotAngleOffset = dotAngleOffset
    , pendingLocks = level
    , phase = phase
    , seed = seed
    }


restartCurrentLevel : Model -> Model
restartCurrentLevel model =
    initHelp
        { level = model.level
        , initialSeed = model.seed
        , phase = WaitingForUserInput
        }


initNextLevel : Model -> Model
initNextLevel model =
    initHelp
        { level = model.level + 1
        , initialSeed = model.seed
        , phase = NextLevelEntered
        }


updateOnUserInput : Model -> Model
updateOnUserInput model =
    case model.phase of
        WaitingForUserInput ->
            { model | phase = Rotating }

        Rotating ->
            if isPinOverDot model then
                if model.pendingLocks == 1 then
                    { model | pendingLocks = 0, phase = LevelCompleted }

                else
                    let
                        ( dotAngleOffset, seed ) =
                            Random.step randomDotAngleOffset model.seed
                    in
                    { level = model.level
                    , phase = Rotating
                    , seed = seed
                    , pinRotatedFor = 0
                    , pinStartingAngle = pinAngle model
                    , pinAngularDirection = oppositeAngularDirection model.pinAngularDirection
                    , dotAngleOffset = dotAngleOffset
                    , pendingLocks = model.pendingLocks - 1
                    }

            else
                { model | phase = LevelFailed }

        LevelCompleted ->
            model

        LevelFailed ->
            model

        NextLevelEntered ->
            model


step : Float -> Model -> Model
step dt model =
    case model.phase of
        WaitingForUserInput ->
            model

        Rotating ->
            { model | pinRotatedFor = model.pinRotatedFor + dt }
                |> (\m ->
                        if hasPinGoneBeyondDot m then
                            { m | phase = LevelFailed }

                        else
                            m
                   )

        LevelCompleted ->
            model

        LevelFailed ->
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
                ( LevelCompleted, "slideOutLeft" ) ->
                    initNextLevel model

                ( LevelFailed, "headShake" ) ->
                    restartCurrentLevel model

                ( NextLevelEntered, "slideInRight" ) ->
                    { model | phase = WaitingForUserInput }

                _ ->
                    model
            , Cmd.none
            )

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
        vm : ViewModel
        vm =
            { bgColor = getBGColor model.phase
            , level = model.level
            , pendingLocks = model.pendingLocks
            , pinAngle = pinAngle model
            , dotAngle = Just (dotAngle model)
            , classes = []
            , style = ""
            , lockHandleClasses = ( [], "" )
            }
    in
    case model.phase of
        WaitingForUserInput ->
            vm

        Rotating ->
            vm

        LevelCompleted ->
            { vm
                | lockHandleClasses =
                    ( []
                    , "animation: popLockHandle; animation-fill-mode: both; animation-duration: 1000ms"
                    )
                , dotAngle = Nothing
                , classes = [ cnAnimated, cnSlideOutLeft ]
                , style = "animation-delay: 1000ms; animation-duration: 500ms"
            }

        LevelFailed ->
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
                LevelFailed ->
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
