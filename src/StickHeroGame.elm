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
    }


type Phase
    = Waiting
    | Clicking


init : () -> ( Model, Cmd Msg )
init () =
    ( { clock = 0
      , phase = Waiting
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
            ( case model.phase of
                Waiting ->
                    model

                Clicking ->
                    { model | clock = model.clock + delta }
            , Cmd.none
            )

        OnKeyDown key ->
            ( model, Cmd.none )

        OnKeyUp key ->
            ( model, Cmd.none )


transitionSpeed =
    0.05


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
                    , translateF2 ( -model.clock * transitionSpeed, 0 )
                    ]
                ]
                [ viewWalls initWalls
                , viewHero
                ]
            , group [ opacity 0.01 ]
                [ circle 100 [ fill wBlue ]
                , circle 1 [ fill wPink ]
                ]
            ]
        ]


wallWidthRange =
    ( 15, 25 )


doubleScoreSquareWidth =
    lerpRange wallWidthRange 0 / 2


initialWallWidth =
    lerpRange wallWidthRange 1


wallGapRange =
    ( 40, 70 )


type alias Wall =
    { x : Float, w : Float }


initialWall : Wall
initialWall =
    Wall 0 initialWallWidth


newWallAfter : Wall -> { gap : Float, width : Float } -> Wall
newWallAfter prevWall attr =
    { x = prevWall.x + prevWall.w / 2 + attr.gap + attr.width / 2
    , w = attr.width
    }


type Walls
    = Walls Wall (Array Wall)


wallsLast : Walls -> Wall
wallsLast (Walls c after) =
    Array.get (Array.length after - 1) after
        |> Maybe.withDefault c


wallsAppendIn : Walls -> Wall -> Walls
wallsAppendIn (Walls c after) last =
    Walls c (Array.push last after)


initWalls : Walls
initWalls =
    Walls initialWall Array.empty
        |> Random.constant
        |> applyN 1000 addRandomWall
        |> stepWithInitialSeed 0


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


viewHero =
    noView
