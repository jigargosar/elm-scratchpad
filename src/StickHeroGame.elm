module StickHeroGame exposing (main)

import Browser.Events
import Json.Decode as JD
import Random exposing (Seed)
import Svg
import Utils exposing (..)


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
    | Walking Stick
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

        Walking stick ->
            Just stick

        Falling stick ->
            Just stick

        Transitioning ->
            Nothing

        Over ->
            Nothing


ensureSufficientWalls : Phase -> Model -> Model
ensureSufficientWalls prevPhase model =
    if model.phase == Waiting && prevPhase /= Waiting then
        case wallsEnsureSufficient model.walls of
            Nothing ->
                model

            Just gen ->
                let
                    ( walls, seed ) =
                        Random.step gen model.seed
                in
                { model | walls = walls, seed = seed }

    else
        model


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
            { model
                | phase =
                    if stick == turnedStick then
                        Walking turnedStick

                    else
                        Turning turnedStick
            }

        Walking stick ->
            let
                ( maxHeroX, mbNextWalls ) =
                    case wallsSelectNextTouchingEndOfStick stick model.walls of
                        Nothing ->
                            ( stickX2 stick
                            , Nothing
                            )

                        Just nextWalls ->
                            ( wallsCurrentCX nextWalls
                            , Just nextWalls
                            )

                heroX =
                    model.heroX + dt * walkingSpeed |> atMost maxHeroX
            in
            if heroX == model.heroX then
                case mbNextWalls of
                    Nothing ->
                        { model | phase = Falling stick }

                    Just nextWalls ->
                        { model
                            | walls = nextWalls
                            , sticks = stick :: model.sticks
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

            else
                { model | xOffset = xOffset }

        Falling stick ->
            let
                maxHeroY =
                    100

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
                |> ensureSufficientWalls model.phase
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
        , viewDebugInfo model |> always noView
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


viewSvg : Model -> Html msg
viewSvg model =
    let
        ( viewportWidth, viewportHeight ) =
            ( 200, 200 )
    in
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
            [ model.sticks
                |> List.map viewStick
                |> group []
            , model.phase
                |> stickFromPhase
                |> maybeView viewStick
            , model.walls
                |> wallsToList
                |> List.map viewWall
                |> group []
            , viewHero model.heroX model.heroY
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


wallContainsX : Float -> Wall -> Bool
wallContainsX x wall =
    x >= wallX1 wall && x <= wallX2 wall


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


newWallAfter : Wall -> GapWidth -> Wall
newWallAfter prevWall attr =
    { x = wallX2 prevWall + attr.gap + attr.width / 2
    , w = attr.width
    }


randomWallSequenceAfter : Int -> Wall -> Generator (List Wall)
randomWallSequenceAfter n firstWall =
    let
        reducer : GapWidth -> ( Wall, List Wall ) -> ( Wall, List Wall )
        reducer gw ( prevWall, acc ) =
            let
                wall =
                    newWallAfter prevWall gw
            in
            ( wall, wall :: acc )

        accToReturn : ( Wall, List Wall ) -> List Wall
        accToReturn =
            second >> List.reverse

        fromGapWidthList : List GapWidth -> List Wall
        fromGapWidthList =
            List.foldl reducer ( firstWall, [] ) >> accToReturn
    in
    Random.list n randomGapWidth
        |> Random.map fromGapWidthList


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


sufficientAfterWallCount =
    20


minimumAfterWallCount =
    sufficientAfterWallCount // 2


wallsRandom : Generator Walls
wallsRandom =
    randomWallSequenceAfter sufficientAfterWallCount initialWall
        |> Random.map (Walls [] initialWall)


wallsEnsureSufficient : Walls -> Maybe (Generator Walls)
wallsEnsureSufficient (Walls before c after) =
    if List.length after <= minimumAfterWallCount then
        let
            lastWall =
                after
                    |> listLast
                    |> Maybe.withDefault c
        in
        randomWallSequenceAfter sufficientAfterWallCount lastWall
            |> Random.map
                (\afterLast ->
                    Walls before c (after ++ afterLast)
                )
            |> Just

    else
        Nothing


wallsSelectNextTouchingEndOfStick : Stick -> Walls -> Maybe Walls
wallsSelectNextTouchingEndOfStick stick =
    wallsSelectNextContainingX (stickX2 stick)


wallsSelectNextContainingX : Float -> Walls -> Maybe Walls
wallsSelectNextContainingX x =
    wallsSelectNext >> maybeFilter (wallsCurrentContainsX x)


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


wallsCurrentContainsX : Float -> Walls -> Bool
wallsCurrentContainsX x =
    wallsCurrent >> wallContainsX x


wallsCurrent : Walls -> Wall
wallsCurrent (Walls _ c _) =
    c


wallsToList : Walls -> List Wall
wallsToList (Walls before c after) =
    before ++ [ c ] ++ after
