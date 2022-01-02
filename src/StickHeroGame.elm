module StickHeroGame exposing (main)

import Utils exposing (..)


width =
    200


height =
    100


wallHeight =
    50


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
