module StickHeroGame exposing (main)

import Random
import Utils exposing (..)


width =
    200


height =
    100


wallHeight =
    50


initialWallWidth =
    30


type alias Wall =
    { x : Float, w : Float }


initialWall : Wall
initialWall =
    Wall 0 initialWallWidth


initialWallGap : Float
initialWallGap =
    75


type Walls
    = Walls Wall (List Wall)


initWalls : Walls
initWalls =
    Walls initialWall []
        |> Random.constant
        |> applyN 10 addRandomWall
        |> stepWithInitialSeed 0


randomWallAfter : Wall -> Generator Wall
randomWallAfter { x, w } =
    Random.map2 Wall
        (randomWallGap |> Random.map (\gap -> x + gap))
        randomWallWidth


randomWallWidth : Generator Float
randomWallWidth =
    Random.constant initialWallWidth


randomWallGap : Generator Float
randomWallGap =
    Random.constant initialWallGap


addRandomWall : Generator Walls -> Generator Walls
addRandomWall =
    Random.andThen
        (\(Walls l p) ->
            randomWallAfter l
                |> Random.map (\n -> Walls n (l :: p))
        )


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
