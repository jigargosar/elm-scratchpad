module StickHeroGame exposing (main)

import Array exposing (Array)
import Random
import Svg
import Utils exposing (..)


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


type Walls
    = Walls Wall (Array Wall)


initWalls : Walls
initWalls =
    Walls initialWall Array.empty
        |> Random.constant
        |> applyN 1000 addRandomWall
        |> stepWithInitialSeed 0


addRandomWall : Generator Walls -> Generator Walls
addRandomWall =
    Random.andThen
        (\(Walls c after) ->
            randomWallAfter (Array.get (Array.length after - 1) after |> Maybe.withDefault c)
                |> Random.map (\n -> Walls c (Array.push n after))
        )


randomWallAfter : Wall -> Generator Wall
randomWallAfter p =
    Random.map2
        (\wallGap wallWidth ->
            { x = p.x + p.w / 2 + wallGap + wallWidth / 2
            , w = wallWidth
            }
        )
        (randomFloatT wallGapRange)
        (randomFloatT wallWidthRange
         --|> always (Random.constant (first wallWidthRange))
        )


wallsToList : Walls -> List Wall
wallsToList (Walls c after) =
    c :: Array.toList after


main =
    let
        width =
            200

        height =
            150
    in
    div []
        [ basicStylesNode
        , basicSvg
            [ viewBoxC width height
            , sMaxWidth "500px"
            ]
            [ group
                [ xf
                    [ --
                      mv2 (width / -3) 0
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
