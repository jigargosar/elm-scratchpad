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
                ( nextHeroX, mbNextWalls ) =
                    case wallsTouchingEndOfStick stick model.walls of
                        Nothing ->
                            let
                                maxHeroX =
                                    stickX2 stick

                                heroX =
                                    model.heroX + dt * walkingSpeed |> atMost maxHeroX
                            in
                            ( heroX, Nothing )

                        Just nextWalls ->
                            let
                                maxHeroX =
                                    wallsCurrentCX nextWalls

                                heroX =
                                    model.heroX + dt * walkingSpeed |> atMost maxHeroX
                            in
                            ( heroX, Just nextWalls )
            in
            case wallsTouchingEndOfStick stick model.walls of
                Nothing ->
                    let
                        maxHeroX =
                            stickX2 stick

                        heroX =
                            model.heroX + dt * walkingSpeed |> atMost maxHeroX
                    in
                    { model
                        | heroX = heroX
                        , phase =
                            if heroX == model.heroX then
                                Falling stick

                            else
                                Walking stick
                    }

                Just nextWalls ->
                    let
                        maxHeroX =
                            wallsCurrentCX nextWalls

                        heroX =
                            model.heroX + dt * walkingSpeed |> atMost maxHeroX
                    in
                    if heroX == model.heroX then
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
                    model.xOffset + transitionSpeed * dt

                maxXOffset =
                    model.heroX
            in
            if xOffset >= maxXOffset then
                { model | xOffset = maxXOffset, phase = Waiting }

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


type Phase
    = Waiting
    | Stretching Stick
    | Turning Stick
    | Walking Stick
    | Falling Stick
    | Transitioning
    | Over


init : () -> ( Model, Cmd Msg )
init () =
    ( initModelWithSeed <| Random.initialSeed 0
    , Cmd.none
    )


initModelWithSeed : Seed -> Model
initModelWithSeed initialSeed =
    let
        ( walls, seed ) =
            Random.step randomWalls initialSeed
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
        , div
            [ positionFixed
            , bgc wBlack
            , fg wBlue
            , pa "1ch"
            , fontSize "14px"
            ]
            [ text <| Debug.toString model.phase ]
            |> always noView
        ]


viewSvg : Model -> Html msg
viewSvg model =
    let
        ( width, height ) =
            ( 200, 200 )
    in
    basicSvg
        [ viewBoxC width height
        , sMaxWidth "100vw"
        , sMaxHeight "100vh"
        ]
        [ group
            [ xf [ mv2 (width / -3) 0, mvLeft model.xOffset ] ]
            [ model.walls
                |> wallsToList
                |> List.map viewWall
                |> group []
            , model.sticks
                |> List.map viewStick
                |> group []
            , viewHero model.heroX model.heroY
            , case model.phase of
                Waiting ->
                    noView

                Stretching stick ->
                    viewStick stick

                Turning stick ->
                    viewStick stick

                Walking stick ->
                    viewStick stick

                Falling stick ->
                    viewStick stick

                Transitioning ->
                    noView

                Over ->
                    noView
            ]
        , case model.phase of
            Over ->
                group [ xf [ mvUp (height / 4) ] ]
                    [ rect width (height / 4) [ fill black, opacity 0.9 ]
                    , words "RESTART"
                        [ fill wWhite
                        , fontSize "30px"
                        ]
                    ]

            _ ->
                noView
        , group [ opacity 0.01 ]
            [ circle 100 [ fill wBlue ]
            , circle 1 [ fill wPink ]
            ]
            |> always noView
        ]


viewStick : Stick -> Svg msg
viewStick stick =
    polyline
        [ ( 0, 0 )
        , ( stick.len, 0 )
        ]
        [ strokeW 2
        , stroke wGreen_lime
        , stroke wWhite
        , xf [ mv2 stick.x 0, rotateDeg stick.angleDeg ]
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



-- WALL


type alias Wall =
    { x : Float, w : Float }


wallIsXInRange : Float -> Wall -> Bool
wallIsXInRange n { x, w } =
    n >= x - w / 2 && n <= x + w / 2


initialWall : Wall
initialWall =
    Wall 0 initialWallWidth


wallX2 : Wall -> Float
wallX2 { x, w } =
    x + (w / 2)


newWallAfter : Wall -> { gap : Float, width : Float } -> Wall
newWallAfter prevWall attr =
    { x = prevWall.x + prevWall.w / 2 + attr.gap + attr.width / 2
    , w = attr.width
    }



-- WALLS


type Walls
    = Walls (List Wall) Wall (List Wall)


randomWalls : Generator Walls
randomWalls =
    Walls [] initialWall []
        |> Random.constant
        |> applyN 100 addRandomWall


wallsTouchingEndOfStick : Stick -> Walls -> Maybe Walls
wallsTouchingEndOfStick stick (Walls before c after) =
    uncons after
        |> Maybe.andThen
            (\( wall, newAfter ) ->
                if wallIsXInRange (stickX2 stick) wall then
                    Just (Walls (before ++ [ c ]) wall newAfter)

                else
                    Nothing
            )


wallsCurrentX2 : Walls -> Float
wallsCurrentX2 =
    wallsCurrent >> wallX2


wallsCurrentCX : Walls -> Float
wallsCurrentCX =
    wallsCurrent >> .x


wallsCurrent : Walls -> Wall
wallsCurrent (Walls _ c _) =
    c


wallsLast : Walls -> Wall
wallsLast (Walls _ c after) =
    after
        |> List.reverse
        |> List.head
        |> Maybe.withDefault c


wallsAppendIn : Walls -> Wall -> Walls
wallsAppendIn (Walls before c after) last =
    Walls before c (after ++ [ last ])


addRandomWall : Generator Walls -> Generator Walls
addRandomWall =
    Random.andThen
        (\walls ->
            wallsLast walls
                |> randomWallAfter
                |> Random.map (wallsAppendIn walls)
        )


randomWallAfter : Wall -> Generator Wall
randomWallAfter prevWall =
    Random.map2
        (\wallGap wallWidth ->
            newWallAfter prevWall { gap = wallGap, width = wallWidth }
        )
        (randomFloatT wallGapRange)
        (randomFloatT wallWidthRange)


wallsToList : Walls -> List Wall
wallsToList (Walls before c after) =
    before ++ [ c ] ++ after


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


viewHero xOffset yOffset =
    group [ xf [ mv2 xOffset yOffset ] ]
        [ rect
            heroWidth
            heroHeight
            [ fill wBlue
            , xf [ mvUp (heroHeight / 2) ]
            ]
        ]
