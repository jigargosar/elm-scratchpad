module StickHeroGame exposing (main)

import Random
import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes.InPx as Px
import Utils exposing (..)


wallHeight =
    50


wallWidthRange =
    ( 15, 25 )


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
    = Walls Wall (List Wall)


initWalls : Walls
initWalls =
    Walls initialWall []
        |> Random.constant
        |> applyN 1000 addRandomWall
        |> stepWithInitialSeed 3


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
            [ circle 100 [ fill wBlue, opacity 0.05 ]
            , group
                [ xf
                    [--
                     --mv2 (width / -3) 0
                    ]
                ]
                [ viewWalls initWalls
                , viewHero
                ]
            , circle 1 [ fill wPink, opacity 1 ]
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
        , rect 5 3 [ fill wPurple, xf [ mv2 0 (3 / 2) ] ]
        ]


viewHero =
    noView
