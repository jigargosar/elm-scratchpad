module Particles.FireworkSvg exposing (..)

import Utils exposing (..)


main =
    svg
        [ viewBoxC 300 300
        , noFill
        , noStroke
        , bgc black
        ]
        [ trail vZero (vec 100 100)
        ]


trail s e =
    sampleVecFromTo 50 s e
        |> List.map (\p -> circle 10 [ xf [ mv p ] ])
        |> group [ fill white ]
