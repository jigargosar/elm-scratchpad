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
    normSamples 50
        |> List.map
            (\n ->
                let
                    p =
                        vLerp s e n
                in
                circle 5
                    [ fill <| hsla 0.1 1 0.5 n
                    , xf [ mv p ]
                    ]
            )
        |> group []


hsl h s l =
    Color.hsl h s l |> Color.toCssString


hsla h s l a =
    Color.hsla h s l a |> Color.toCssString
