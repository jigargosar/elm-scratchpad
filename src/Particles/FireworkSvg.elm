module Particles.FireworkSvg exposing (..)

import Color
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
        |> List.map (\p -> circle 5 [ xf [ mv p ] ])
        |> group [ fill <| hsl 0.1 0.5 0.5 ]


hsl h s l =
    Color.hsl h s l |> Color.toCssString
