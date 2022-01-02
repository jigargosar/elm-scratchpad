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


initWalls : Walls
initWalls =
    Walls initialWall []
        |> applyN 10 addWall


addWall : Walls -> Walls
addWall (Walls lastWall prevWalls) =
    let
        nextWall =
            Wall (lastWall.x + lastWall.w / 2 + initialWallGap) initialWallWidth
    in
    Walls nextWall (lastWall :: prevWalls)


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
