module StickHeroGame exposing (main)

import Random
import Utils exposing (..)


width =
    200


height =
    100


wallHeight =
    50


wallWidthRange =
    ( 10, 30 )


initialWallWidth =
    lerpRange wallWidthRange 0.5


wallGapRange =
    ( 50, 100 )


initialWallGap =
    lerpRange wallGapRange 0.5


type alias Wall =
    { x : Float, w : Float }


initialWall : Wall
initialWall =
    Wall 0 initialWallWidth


type Walls
    = Walls Wall (List Wall)


initWalls : Walls
initWalls =
    Walls initialWall []
        |> Random.constant
        |> applyN 10 addRandomWall
        |> stepWithInitialSeed 0


addRandomWall : Generator Walls -> Generator Walls
addRandomWall =
    Random.andThen
        (\(Walls l p) ->
            randomWallAfter l
                |> Random.map (\n -> Walls n (l :: p))
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
        (randomFloatT wallWidthRange)


wallsToList : Walls -> List Wall
wallsToList (Walls last prev) =
    last :: prev


main =
    div []
        [ basicStylesNode
        , basicSvg [ viewBoxC width height, sMaxWidth "500px" ]
            [ group [ xf [ mv2 (width / -3) 0 ] ]
                [ initWalls
                    |> viewWalls
                , viewHero
                ]
            ]
        ]


viewWalls : Walls -> Svg msg
viewWalls walls =
    walls
        |> wallsToList
        |> List.map viewWall
        |> group []


viewWall { x, w } =
    rect w wallHeight [ fill white, xf [ mv2 x (wallHeight / 2) ] ]


viewHero =
    noView
