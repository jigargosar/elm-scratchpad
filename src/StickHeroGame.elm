module StickHeroGame exposing (main)

import Random exposing (Seed)
import TypedSvg
import TypedSvg.Attributes
import Utils exposing (..)



{-
   INDENTATION FIX FOR ELM-FORMAT
    Specs:
    * clone Stick Hero
    * codepen: https://codepen.io/HunorMarton/pen/xxOMQKg
    * youtube: https://www.youtube.com/watch?v=eue3UdFvwPo

    Pending:
    [x] use pointer-event pointer-down/up to make it work in mobile mode.
    [x] play it on your mobile.
    [x] fix pointer up not being detected.
    [x] prevent screen scrolling on phone.
    * list visuals to clone
    * list steps to take before you can send it to Ojas.
        * deploy
        * polish?
    * export repo to git hub.
    * check notes.md

    LATER/NEVER:
    * on fail there should be visible gap.
        * perhaps make walls appear thinner than actual,
            * doesn't work, there will be a gap at stick start.
        * can't play with stick length, will cause overflow or underflow.
        * ideally on fail, we want to show smaller stick, that's all.


    DONE:
    [x] simplify ensureSufficientWalls
    [x] Score on top right
    [x] split walking phase
    [x] DOUBLE SCORE indicator.
    [x] Starting Instructions: "Hold down the mouse to stretch the stick"
    [x] click input
    [x] make game more difficult by reducing max/average wall width.
    [x] make svg expand to full screen, preserve aspect ratio to clone responsiveness.
-}


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- CONSTANTS


stretchSpeed =
    0.1


walkingSpeed =
    0.15


heroFallingSpeed =
    0.3


transitionSpeed =
    0.15


turnSpeed =
    0.4


heroWidth =
    10


heroHeight =
    heroWidth * 2


doubleScoreSquareWidth =
    heroWidth / 2


wallWidthRange =
    ( heroWidth * 1.2, heroWidth * 2.5 )


wallGapRange =
    ( heroWidth * 2.5, heroWidth * 10 )


initialWallWidth =
    lerpRange wallWidthRange 0.5



-- MODEL


type alias Model =
    { score : Int
    , phase : Phase
    , walls : Walls
    , heroX : Float
    , heroY : Float
    , sticks : List Stick
    , xOffset : Float
    , seed : Seed
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initModelWithSeed <| Random.initialSeed 0
    , Cmd.none
    )


forceRestart : Model -> Model
forceRestart { seed } =
    initModelWithSeed seed


initModelWithSeed : Seed -> Model
initModelWithSeed initialSeed =
    let
        ( walls, seed ) =
            Random.step wallsRandom initialSeed
    in
    { score = 0
    , phase = Waiting
    , walls = walls
    , heroX = 0
    , heroY = 0
    , sticks = []
    , xOffset = 0
    , seed = seed
    }


type Phase
    = Waiting
    | Stretching Stick
    | Turning Stick
    | WalkingToEndOfStick Stick
    | WalkingToCenterOfWall WallTouch Walls
    | Falling Stick
    | Transitioning
    | Over


startStretchingOnUserInput : Model -> Model
startStretchingOnUserInput model =
    case model.phase of
        Waiting ->
            { model
                | phase =
                    Stretching (initStretchingStick (wallsCurrentX2 model.walls))
            }

        _ ->
            model


stopStretchingOnUserInput : Model -> Model
stopStretchingOnUserInput model =
    case model.phase of
        Stretching stick ->
            { model | phase = Turning stick }

        _ ->
            model


cancelOrStopStretchingOnUserInput : Model -> Model
cancelOrStopStretchingOnUserInput model =
    case model.phase of
        Stretching stick ->
            if stick.len < 10 then
                { model | phase = Waiting }

            else
                { model | phase = Turning stick }

        _ ->
            model


isWaitingForFirstTime : Model -> Bool
isWaitingForFirstTime model =
    model.phase == Waiting && List.isEmpty model.sticks


shouldShowDoubleScoreIndicator : Model -> Bool
shouldShowDoubleScoreIndicator model =
    case model.phase of
        WalkingToCenterOfWall TouchingCentralRegion _ ->
            True

        _ ->
            False


stickFromPhase : Phase -> Maybe Stick
stickFromPhase phase =
    case phase of
        Waiting ->
            Nothing

        Stretching stick ->
            Just stick

        Turning stick ->
            Just stick

        WalkingToEndOfStick stick ->
            Just stick

        WalkingToCenterOfWall _ _ ->
            Nothing

        Falling stick ->
            Just stick

        Transitioning ->
            Nothing

        Over ->
            Nothing


step : Float -> Model -> Model
step dt model =
    case model.phase of
        Waiting ->
            model

        Stretching stick ->
            { model | phase = Stretching <| stretchStick dt stick }

        Turning stick ->
            let
                turnedStick =
                    turnStick dt stick
            in
            if stick == turnedStick then
                --{ model | phase = Walking turnedStick }
                case wallsSelectNextTouchingX (stickX2 stick) model.walls of
                    Nothing ->
                        { model | phase = WalkingToEndOfStick turnedStick }

                    Just ( wallTouch, walls ) ->
                        { model
                            | phase = WalkingToCenterOfWall wallTouch walls
                            , sticks = stick :: model.sticks
                            , score =
                                model.score
                                    + (case wallTouch of
                                        TouchingCentralRegion ->
                                            2

                                        TouchingNonCentralRegion ->
                                            1
                                      )
                        }

            else
                { model | phase = Turning turnedStick }

        WalkingToEndOfStick stick ->
            let
                maxHeroX =
                    stickX2 stick

                heroX =
                    model.heroX + dt * walkingSpeed |> atMost maxHeroX
            in
            if heroX == model.heroX then
                { model | phase = Falling stick }

            else
                { model | heroX = heroX }

        WalkingToCenterOfWall _ walls ->
            let
                maxHeroX =
                    wallsCurrentCX walls

                heroX =
                    model.heroX + dt * walkingSpeed |> atMost maxHeroX
            in
            if heroX == model.heroX then
                { model
                    | walls = walls
                    , phase = Transitioning
                }

            else
                { model | heroX = heroX }

        Transitioning ->
            let
                xOffset =
                    model.xOffset + transitionSpeed * dt |> atMost model.heroX
            in
            if xOffset == model.xOffset then
                { model | phase = Waiting }
                    |> ensureSufficientWalls

            else
                { model | xOffset = xOffset }

        Falling stick ->
            let
                maxHeroY =
                    viewportHeight / 2

                heroY =
                    model.heroY
                        |> add (dt * heroFallingSpeed)
                        |> atMost maxHeroY

                updatedStick =
                    fallStick dt stick
            in
            if heroY == model.heroY && updatedStick == stick then
                { model
                    | phase = Over
                    , sticks = updatedStick :: model.sticks
                }

            else
                { model
                    | phase = Falling updatedStick
                    , heroY = heroY
                }

        Over ->
            model


ensureSufficientWalls : Model -> Model
ensureSufficientWalls model =
    case wallsEnsureSufficient model.walls of
        Nothing ->
            model

        Just gen ->
            let
                ( walls, seed ) =
                    Random.step gen model.seed
            in
            { model | walls = walls, seed = seed }


type Msg
    = NOP
    | OnClampedDelta Float
    | OnKeyDown KeyEvent
    | OnKeyUp String
    | OnPointerDown
    | OnPointerCancel
    | OnPointerUp
    | RestartClicked


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ onAnimationFrameClampedDelta OnClampedDelta
    , onBrowserKeyDown OnKeyDown
    , onBrowserKeyUp OnKeyUp
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnClampedDelta delta ->
            ( step delta model, Cmd.none )

        OnKeyDown e ->
            ( if e.repeat then
                model

              else
                case ( model.phase, e.key ) of
                    ( Waiting, " " ) ->
                        startStretchingOnUserInput model

                    ( Over, " " ) ->
                        forceRestart model

                    _ ->
                        if List.member e.key [ "r", "Enter" ] then
                            forceRestart model

                        else
                            model
            , Cmd.none
            )

        OnKeyUp key ->
            ( case key of
                " " ->
                    stopStretchingOnUserInput model

                _ ->
                    model
            , Cmd.none
            )

        OnPointerDown ->
            ( startStretchingOnUserInput model, Cmd.none )

        OnPointerCancel ->
            ( cancelOrStopStretchingOnUserInput model, Cmd.none )

        OnPointerUp ->
            ( stopStretchingOnUserInput model, Cmd.none )

        RestartClicked ->
            ( forceRestart model, Cmd.none )


view : Model -> Document Msg
view model =
    Document "Stick Hero"
        [ basicStylesNode
        , basicSvg
            [ viewBoxC viewportWidth viewportHeight
            , positionAbsolute
            , absoluteFill

            --, sWidth "100vw"
            --, sHeight "100vh"
            , sMaxWidth "100vw"
            , sMaxHeight "100vh"
            , disableContextMenu NOP

            --, style "touch-action" "none"
            , notifyPointerDown OnPointerDown
            , notifyPointerUp OnPointerUp
            , notifyPointerCancel OnPointerCancel
            ]
            [ viewBackground -model.xOffset
            , group
                [ xf
                    [ mvLeft (viewportWidth / 3)
                    , mvLeft model.xOffset
                    ]
                ]
                [ viewSticks model.sticks
                , viewStickFromPhase model.phase
                , viewWalls model.walls
                , viewHero model.heroX model.heroY
                ]
            , viewScore model.score
            , viewStartingInstructions (isWaitingForFirstTime model)
            , viewDoubleScoreIndicator (shouldShowDoubleScoreIndicator model)
            , viewRestartGameOverlay (model.phase == Over)
            ]
        ]


viewBackground xOffset =
    group []
        [ TypedSvg.polygon
            [ TypedSvg.Attributes.points (hillPoints hill1 xOffset)
            , fill wGreen_lime
            , stroke wBlue
            , transforms [ translateF2 ( 0, viewportHeight / 2 ) ]
            ]
            []
        , TypedSvg.polygon
            [ TypedSvg.Attributes.points (hillPoints hill2 (xOffset + 150))
            , fill wGreen2_sea
            , stroke wBlue
            , transforms [ translateF2 ( 0, viewportHeight / 2 ) ]
            ]
            []
        ]


type alias Hill =
    { amplitude : Float
    , frequency : Float
    , baseHeight : Float
    }


yFromXOfHill : Hill -> Float -> Float
yFromXOfHill { amplitude, frequency, baseHeight } x =
    (sin (x * (turns 1 / frequency)) * amplitude)
        |> add -baseHeight


hill1 : Hill
hill1 =
    { amplitude = 16, frequency = 220, baseHeight = 90 }


hill2 : Hill
hill2 =
    { amplitude = 8, frequency = 250, baseHeight = 60 }


hillPoints : Hill -> Float -> List Float2
hillPoints hill xOffset =
    let
        xMin =
            ---viewportWidth * 10
            0

        xMax =
            --viewportWidth * 10
            viewportWidth

        inBetween =
            List.range xMin xMax
                |> List.map toFloat
                |> List.map (\x -> ( x, yFromXOfHill hill (x + xOffset) ))
    in
    ( xMin, 0 ) :: inBetween ++ [ ( xMax, 0 ) ]


viewportWidth =
    200


viewportHeight =
    viewportWidth * 2


transitionOpacity =
    style "transition" "opacity 500ms ease-in-out"


viewStartingInstructions : Bool -> Svg msg
viewStartingInstructions show =
    words "Hold down the mouse to stretch out the stick"
        [ opacityFromBool show
        , transitionOpacity
        , fill white
        , transforms [ "translateY(-30%)" ]
        , fontSize "10px"
        ]


viewDoubleScoreIndicator : Bool -> Svg msg
viewDoubleScoreIndicator active =
    words "DOUBLE SCORE"
        [ opacityFromBool active
        , transitionOpacity
        , fill white
        , transforms [ "translateY(-30%)" ]
        , fontSize "10px"
        ]


viewScore : Int -> Svg msg
viewScore score =
    words (fromInt score)
        [ fill wWhite
        , wordsAlignYTop
        , wordsAlignXRight
        , transforms [ "translate(calc(50% - 1ch),-50%)" ]
        , fontSize "20px"
        ]


viewRestartGameOverlay : Bool -> Svg Msg
viewRestartGameOverlay active =
    group
        [ opacityFromBool active
        , pointerEventsFromBool active
        , transitionOpacity
        , notifyClick RestartClicked
        ]
        [ -- hack: using large area for capturing restart clicks
          -- since svg element can be larger than vp
          square (max viewportWidth viewportHeight |> mul 3) [ fill transparent ]
        , group
            [ xf [ mvUp (viewportHeight / 4) ]
            ]
            [ rect viewportWidth (viewportHeight / 4) [ fill black, opacity 0.9 ]
            , words "RESTART" [ fill wWhite, fontSize "30px" ]
            ]
        ]


viewSticks : List Stick -> Svg msg
viewSticks sticks =
    sticks
        |> List.map viewStick
        |> group []


viewStickFromPhase : Phase -> Svg msg
viewStickFromPhase phase =
    phase
        |> stickFromPhase
        |> maybeView viewStick


viewWalls : Walls -> Svg msg
viewWalls walls =
    walls
        |> wallsToList
        |> List.map viewWall
        |> group []


viewHero : Float -> Float -> Svg msg
viewHero xOffset yOffset =
    group [ xf [ mv2 xOffset yOffset ] ]
        [ rectB heroWidth heroHeight [ fill wBlue ] ]


type alias Stick =
    { x : Float, len : Float, angleDeg : Float }


initStretchingStick : Float -> Stick
initStretchingStick x =
    { x = x, len = 0, angleDeg = -90 }


stretchStick : Float -> Stick -> Stick
stretchStick dt stick =
    { stick | len = stick.len + dt * stretchSpeed }


fallStick : Float -> Stick -> Stick
fallStick dt stick =
    { stick
        | angleDeg =
            stick.angleDeg
                + dt
                * turnSpeed
                |> clamp 0 90
    }


turnStick : Float -> Stick -> Stick
turnStick dt stick =
    { stick
        | angleDeg =
            stick.angleDeg
                + dt
                * turnSpeed
                |> clamp -90 0
    }


stickX2 : Stick -> Float
stickX2 { x, len } =
    x + len


viewStick : Stick -> Svg msg
viewStick stick =
    let
        sw =
            2
    in
    polyline
        [ ( 0, 0 )
        , ( stick.len, 0 )
        ]
        [ strokeW sw
        , stroke wGreen_lime
        , stroke wWhite
        , xf [ mv2 stick.x (sw / 2), rotateDeg stick.angleDeg ]
        ]



-- WALL


type alias Wall =
    { x : Float, w : Float }


initialWall : Wall
initialWall =
    Wall 0 initialWallWidth


wallTouchAtX : Float -> Wall -> Maybe WallTouch
wallTouchAtX x wall =
    if wallContainsX x wall then
        Just
            (if wallCentralRegionContainsX x wall then
                TouchingCentralRegion

             else
                TouchingNonCentralRegion
            )

    else
        Nothing


type WallTouch
    = TouchingCentralRegion
    | TouchingNonCentralRegion


wallContainsX : Float -> Wall -> Bool
wallContainsX x wall =
    x >= wallX1 wall && x <= wallX2 wall


wallCentralRegionContainsX : Float -> Wall -> Bool
wallCentralRegionContainsX x wall =
    let
        hw =
            doubleScoreSquareWidth / 2
    in
    x >= wall.x - hw && x <= wall.x + hw


wallX1 : Wall -> Float
wallX1 { x, w } =
    x - (w / 2)


wallX2 : Wall -> Float
wallX2 { x, w } =
    x + (w / 2)


type alias GapWidth =
    { gap : Float, width : Float }


randomGapWidth : Generator GapWidth
randomGapWidth =
    Random.map2
        GapWidth
        (randomFloatT wallGapRange)
        (randomFloatT wallWidthRange)


newWallAfter : Float -> GapWidth -> Wall
newWallAfter prevWallX2 attr =
    { x = prevWallX2 + attr.gap + attr.width / 2
    , w = attr.width
    }


wallSequenceFromGapWidths : Float -> List GapWidth -> List Wall
wallSequenceFromGapWidths firstWallX2 =
    let
        reducer : GapWidth -> ( Float, List Wall ) -> ( Float, List Wall )
        reducer gw ( prevWallX2, acc ) =
            let
                wall =
                    newWallAfter prevWallX2 gw
            in
            ( wallX2 wall, wall :: acc )
    in
    List.foldl reducer ( firstWallX2, [] )
        >> second
        >> List.reverse


randomWallSequenceAfter : Int -> Float -> Generator (List Wall)
randomWallSequenceAfter n firstWallX2 =
    Random.list n randomGapWidth
        |> Random.map (wallSequenceFromGapWidths firstWallX2)


viewWall : Wall -> Svg msg
viewWall wall =
    let
        wallHeight =
            viewportHeight / 2

        wallWidth =
            wall.w
    in
    group [ xf [ mvRight wall.x ] ]
        [ rectT wallWidth wallHeight [ fill white ]
        , squareT doubleScoreSquareWidth [ fill wPurple ]
        ]



-- WALLS


type Walls
    = Walls (List Wall) Wall (List Wall)


minimumAfterWallCount =
    10


wallsRandom : Generator Walls
wallsRandom =
    randomWallSequenceAfter (minimumAfterWallCount * 2) (wallX2 initialWall)
        |> Random.map (Walls [] initialWall)


wallsEnsureSufficient : Walls -> Maybe (Generator Walls)
wallsEnsureSufficient walls =
    if wallsCountAfter walls <= minimumAfterWallCount then
        randomWallSequenceAfter (minimumAfterWallCount * 2) (wallsLastX2 walls)
            |> Random.map (wallsAppendIn walls)
            |> Just

    else
        Nothing


wallsCountAfter : Walls -> Int
wallsCountAfter (Walls _ _ after) =
    List.length after


wallsLastX2 : Walls -> Float
wallsLastX2 =
    wallsLast >> wallX2


wallsAppendIn : Walls -> List Wall -> Walls
wallsAppendIn (Walls b c a) aa =
    Walls b c (a ++ aa)


wallsLast : Walls -> Wall
wallsLast (Walls _ c after) =
    after
        |> listLast
        |> Maybe.withDefault c


wallsSelectNextTouchingX : Float -> Walls -> Maybe ( WallTouch, Walls )
wallsSelectNextTouchingX x =
    wallsSelectNext
        >> Maybe.andThen
            (\walls ->
                wallsCurrent walls
                    |> wallTouchAtX x
                    |> Maybe.map (pairTo walls)
            )


wallsSelectNext : Walls -> Maybe Walls
wallsSelectNext (Walls b c a) =
    uncons a
        |> Maybe.map (\( nc, na ) -> Walls (b ++ [ c ]) nc na)


wallsCurrentX2 : Walls -> Float
wallsCurrentX2 =
    wallsCurrent >> wallX2


wallsCurrentCX : Walls -> Float
wallsCurrentCX =
    wallsCurrent >> .x


wallsCurrent : Walls -> Wall
wallsCurrent (Walls _ c _) =
    c


wallsToList : Walls -> List Wall
wallsToList (Walls before c after) =
    before ++ [ c ] ++ after
