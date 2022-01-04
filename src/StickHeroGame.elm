module StickHeroGame exposing (main)

import Browser.Events
import Json.Decode as JD
import Random exposing (Seed)
import Svg
import Utils exposing (..)



{-
      Specs:
      * clone Stick Hero
           * codepen: https://codepen.io/HunorMarton/pen/xxOMQKg
           * youtube: https://www.youtube.com/watch?v=eue3UdFvwPo

   Pending:
   [x] simplify ensureSufficientWalls
   [x] Score on top right
   [x] split walking phase
   * DOUBLE SCORE indicator.
   * Starting Instructions: "Hold down then mouse to stretch the stick"
   * click input
-}


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- CONSTANTS


walkingSpeed =
    0.1


heroFallingSpeed =
    0.1


turnSpeed =
    0.2


stretchSpeed =
    0.1


transitionSpeed =
    0.1


heroWidth =
    10


heroHeight =
    heroWidth * 2


doubleScoreSquareWidth =
    heroWidth / 2


wallWidthRange =
    ( heroWidth * 1.5, heroWidth * 4 )


wallGapRange =
    ( heroWidth * 1.5, heroWidth * 8 )


initialWallWidth =
    lerpRange wallWidthRange 0.5



-- MODEL


type alias Model =
    { clock : Float
    , score : Int
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


initModelWithSeed : Seed -> Model
initModelWithSeed initialSeed =
    let
        ( walls, seed ) =
            Random.step wallsRandom initialSeed
    in
    { clock = 0
    , score = 0
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


addDelta : Float -> Model -> Model
addDelta delta model =
    { model | clock = model.clock + delta }


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
                                        TouchingCenteralRegion ->
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

        WalkingToCenterOfWall walls ->
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta (clamp 0 100 >> OnClampedDelta)
    , Browser.Events.onKeyDown (JD.map OnKeyDown keyEventDecoder)
    , Browser.Events.onKeyUp (JD.map OnKeyUp keyDecoder)
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnClampedDelta delta ->
            ( model
                |> step delta
                |> addDelta delta
            , Cmd.none
            )

        OnKeyDown e ->
            ( case ( model.phase, e.key, e.repeat ) of
                ( Waiting, " ", False ) ->
                    { model | phase = Stretching (initStretchingStick (wallsCurrentX2 model.walls)) }

                ( Over, " ", False ) ->
                    initModelWithSeed model.seed

                _ ->
                    model
            , Cmd.none
            )

        OnKeyUp key ->
            ( case ( model.phase, key ) of
                ( Stretching stick, " " ) ->
                    { model | phase = Turning stick }

                _ ->
                    model
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    Document "Stick Hero"
        [ basicStylesNode
        , viewSvg model
        , (\_ ->
            viewDebugInfo model
          )
            |> always noView
        ]


viewDebugInfo : Model -> Html msg
viewDebugInfo model =
    div
        [ positionFixed
        , bgc wBlack
        , fg wBlue
        , pa "1ch"
        , fontSize "14px"
        ]
        [ text <| Debug.toString model.phase ]


viewportWidth =
    200


viewportHeight =
    200


viewSvg : Model -> Html msg
viewSvg model =
    basicSvg
        [ viewBoxC viewportWidth viewportHeight
        , sMaxWidth "100vw"
        , sMaxHeight "100vh"
        ]
        [ group
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
        , words (fromInt model.score)
            [ fill wWhite
            , wordsAlignYTop
            , wordsAlignXRight
            , transforms [ "translate(calc(50% - 1ch),-50%)" ]
            , fontSize "20px"
            ]
        , case model.phase of
            Over ->
                group [ xf [ mvUp (viewportHeight / 4) ] ]
                    [ rect viewportWidth (viewportHeight / 4) [ fill black, opacity 0.9 ]
                    , words "RESTART"
                        [ fill wWhite
                        , fontSize "30px"
                        ]
                    ]

            _ ->
                noView
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
        [ rect
            heroWidth
            heroHeight
            [ fill wBlue
            , xf [ mvUp (heroHeight / 2) ]
            ]
        ]


type alias Stick =
    { x : Float, len : Float, angleDeg : Float }


initStretchingStick : Float -> Stick
initStretchingStick x =
    let
        dt =
            0
    in
    { x = x, len = dt * stretchSpeed, angleDeg = -90 }


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
                TouchingCenteralRegion

             else
                TouchingNonCentralRegion
            )

    else
        Nothing


type WallTouch
    = TouchingCenteralRegion
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
viewWall { x, w } =
    --rect w wallHeight [ fill white, xf [ mv2 x (wallHeight / 2) ] ]
    group [ xf [ mv2 x 0 ] ]
        [ Svg.rect
            [ attrXF (w / -2)
            , aWidthF w
            , attrYF 0
            , aHeight "50%"
            , fill white
            ]
            []
        , square doubleScoreSquareWidth [ fill wPurple, xf [ mv2 0 (doubleScoreSquareWidth / 2) ] ]
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
