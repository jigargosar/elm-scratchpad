module Particles.FireworkSvg exposing (..)

import Color
import Random exposing (Generator)
import Utils exposing (..)


main =
    svg
        [ viewBoxC 300 300
        , noFill
        , noStroke
        , bgc black
        ]
        [ trail 0.1 vZero (vec 100 100)
        ]


randomHue : Generator Float
randomHue =
    Random.float 0 1


trail h s e =
    normSamples 50
        |> List.map
            (\n ->
                let
                    p =
                        vLerp s e n
                in
                circle 5
                    [ fill <| hsla h 1 0.5 n
                    , xf [ mv p ]
                    ]
            )
        |> group []


hsl h s l =
    Color.hsl h s l |> Color.toCssString


hsla h s l a =
    Color.hsla h s l a |> Color.toCssString
