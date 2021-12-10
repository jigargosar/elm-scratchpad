module Particles.FireworkSvg exposing (..)

import Color
import Random exposing (Generator)
import Svg exposing (Svg)
import Utils exposing (..)


main =
    svg
        [ viewBoxC 300 300
        , noFill
        , noStroke
        , bgc black
        ]
        [ --trail 0.1 vZero (vec 100 100) ,
          group [] trails
        ]


trails : List (Svg msg)
trails =
    Random.step (Random.list 40 randomTrail) (Random.initialSeed 0)
        |> first


randomTrail : Generator (Svg msg)
randomTrail =
    Random.map2
        (particle 1)
        randomVec
        randomHue


particle nl nv h =
    trail h vZero (vScale 100 nv)


randomHue : Generator Float
randomHue =
    Random.float 0 1


trail : Float -> Vec -> Vec -> Svg msg
trail h s e =
    normSamples 50
        |> List.map
            (\n ->
                let
                    p =
                        vLerp s e n
                in
                circle 2
                    [ fill <| hsla h 1 0.5 (n * n * n)
                    , xf [ mv p ]
                    ]
            )
        |> group []


hsl h s l =
    Color.hsl h s l |> Color.toCssString


hsla h s l a =
    Color.hsla h s l a |> Color.toCssString
