module StickHeroGame exposing (main)

import Utils exposing (..)


width =
    200


height =
    100


main =
    div []
        [ basicStylesNode
        , basicSvg [ viewBoxC width height, sMaxWidth "500px" ]
            []
        ]
