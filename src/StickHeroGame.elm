module StickHeroGame exposing (main)

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


addWall : Walls -> Walls
addWall (Walls lastWall prevWalls) =
    let
        nextWall =
            Wall (lastWall.x + initialWallGap) initialWallWidth
    in
    Walls nextWall (lastWall :: prevWalls)


main =
    div []
        [ basicStylesNode
        , basicSvg [ viewBoxC width height, sMaxWidth "500px" ]
            [ [ { x = 0, w = 20 }, { x = 75, w = 20 } ]
                |> List.map viewWall
                |> group []
            , viewHero
            ]
        ]


viewWall { x, w } =
    rect w wallHeight [ fill white, xf [ mv2 x (wallHeight / 2) ] ]


viewHero =
    noView
