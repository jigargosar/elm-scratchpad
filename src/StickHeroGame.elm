module StickHeroGame exposing (main)

import Array exposing (Array)
import Browser.Events
import Json.Decode as JD
import Random
import Svg
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { clock : Float
    , phase : Phase
    , walls : Walls
    , sticks : List Stick
    }


step : Model -> Model
step model =
    case model.phase of
        Waiting ->
            model

        Stretching _ ->
            model

        Turning start stick ->
            let
                angleDeg =
                    turningAngleInDegreesSince start model
            in
            if angleDeg >= 0 then
                case wallTouchingEndOfStick stick model.walls of
                    Just walls ->
                        { model
                            | phase = WalkingToCenterOfWall model.clock walls
                            , sticks = stick :: model.sticks
                        }

                    Nothing ->
                        { model
                            | phase = WalkingToEndOfStick model.clock stick
                            , sticks = stick :: model.sticks
                        }

            else
                model

        WalkingToCenterOfWall start _ ->
            let
                elapsed =
                    model.clock - start
            in
            model

        WalkingToEndOfStick start stick ->
            model


type Phase
    = Waiting
    | Stretching Float
    | Turning Float Stick
    | WalkingToCenterOfWall Float Walls
    | WalkingToEndOfStick Float Stick


init : () -> ( Model, Cmd Msg )
init () =
    let
        initWalls : Walls
        initWalls =
            Walls initialWall Array.empty
                |> Random.constant
                |> applyN 1000 addRandomWall
                |> stepWithInitialSeed 0
    in
    ( { clock = 0
      , phase = Turning 0 (Stick (wallX2 initialWall) 50)
      , walls = initWalls
      , sticks = []
      }
    , Cmd.none
    )


type Msg
    = NOP
    | OnClampedDelta Float
    | OnKeyDown String
    | OnKeyUp String


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta (clamp 0 100 >> OnClampedDelta)
    , Browser.Events.onKeyDown (JD.map OnKeyDown keyDecoder)
    , Browser.Events.onKeyUp (JD.map OnKeyUp keyDecoder)
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnClampedDelta delta ->
            ( { model | clock = model.clock + delta }
                |> step
            , Cmd.none
            )

        OnKeyDown key ->
            ( case ( model.phase, key ) of
                ( Waiting, " " ) ->
                    { model | phase = Stretching model.clock }

                _ ->
                    model
            , Cmd.none
            )

        OnKeyUp key ->
            ( case ( model.phase, key ) of
                ( Stretching start, " " ) ->
                    { model | phase = Turning model.clock (initStickWithStartTime start model) }

                _ ->
                    model
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    Document "App Title"
        [ basicStylesNode
        , let
            ( width, height ) =
                ( 200, 200 )
          in
          basicSvg
            [ viewBoxC width height
            , sMaxWidth "100vw"
            , sMaxHeight "100vh"
            ]
            [ group
                [ xf [ mv2 (width / -3) 0 ]
                , transforms
                    [ "translate(-33.33%,0)"

                    --, translateF2 ( -model.clock * 0.05, 0 )
                    ]
                ]
                [ viewWalls model.walls
                , model.sticks
                    |> List.map (viewStick 0)
                    |> group []
                , case model.phase of
                    WalkingToCenterOfWall start _ ->
                        viewHero ((model.clock - start) * 0.05)

                    _ ->
                        viewHero 0
                , case model.phase of
                    Stretching start ->
                        let
                            stick =
                                initStickWithStartTime start model
                        in
                        viewStick -90 stick

                    Turning start stick ->
                        let
                            angleDeg =
                                turningAngleInDegreesSince start model
                        in
                        viewStick angleDeg stick

                    _ ->
                        noView
                ]
            , group [ opacity 0.01 ]
                [ circle 100 [ fill wBlue ]
                , circle 1 [ fill wPink ]
                ]
            ]
        ]


turningAngleInDegreesSince : Float -> Model -> Float
turningAngleInDegreesSince start model =
    let
        turnSpeed =
            0.05

        elapsed =
            model.clock - start
    in
    -90 + (elapsed * turnSpeed)


initStickWithStartTime : Float -> Model -> Stick
initStickWithStartTime start model =
    let
        elapsed =
            model.clock - start

        stickGrowSpeed =
            0.05

        stickLength =
            elapsed * stickGrowSpeed

        xOffset =
            wallsCurrentX2 model.walls
    in
    Stick xOffset stickLength


viewStick : Float -> Stick -> Svg msg
viewStick angleDeg stick =
    polyline
        [ ( 0, 0 )
        , ( stick.len, 0 )
        ]
        [ strokeW 2
        , stroke wGreen_lime
        , stroke wWhite
        , xf [ mv2 stick.x 0, rotateDeg angleDeg ]
        ]


wallWidthRange =
    ( 15, 25 )


doubleScoreSquareWidth =
    lerpRange wallWidthRange 0 / 2


initialWallWidth =
    lerpRange wallWidthRange 1


wallGapRange =
    ( 40, 70 )


type alias Stick =
    { x : Float, len : Float }


type alias Wall =
    { x : Float, w : Float }


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


type Walls
    = Walls Wall (Array Wall)


wallTouchingEndOfStick : Stick -> Walls -> Maybe Walls
wallTouchingEndOfStick stick walls =
    Debug.todo "todo"


wallsCurrentX2 : Walls -> Float
wallsCurrentX2 =
    wallsCurrent >> wallX2


wallsCurrent : Walls -> Wall
wallsCurrent (Walls c _) =
    c


wallsLast : Walls -> Wall
wallsLast (Walls c after) =
    Array.get (Array.length after - 1) after
        |> Maybe.withDefault c


wallsAppendIn : Walls -> Wall -> Walls
wallsAppendIn (Walls c after) last =
    Walls c (Array.push last after)


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
wallsToList (Walls c after) =
    c :: Array.toList after


viewWalls : Walls -> Svg msg
viewWalls walls =
    walls
        |> wallsToList
        |> List.map viewWall
        |> group []


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


viewHero xOffset =
    let
        w =
            15

        h =
            w
    in
    group [ xf [ mv2 xOffset 0 ] ]
        [ Svg.rect
            [ attrXF (w / -2)
            , aWidthF w
            , attrYF -h
            , aHeightF h
            , fill wBlue
            ]
            []
        ]
