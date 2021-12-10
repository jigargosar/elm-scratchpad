module Particles.FireworkSvg exposing (..)

import Color
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SA
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
        (particle 0.5)
        randomVec
        randomHue


particle nl nv h =
    let
        ( nr, theta ) =
            vToPolar nv

        vInitial =
            vZero

        maxLen =
            100

        e =
            nl
                |> rangeMap ( 0, 0.5 ) ( 0, 1 )
                |> clamp 0 1
                |> vLerp vInitial (vScale maxLen nv)

        s =
            nl
                |> rangeMap ( 0.5, 1 ) ( 0, 1 )
                |> clamp 0 1
                |> vLerp vInitial (vScale maxLen nv)
    in
    trail h
        s
        e
        [ SA.opacity <| fromFloat <| rangeMap ( 0.5, 1 ) ( 1, 0 ) nl
        ]


randomHue : Generator Float
randomHue =
    Random.float 0 1


trail h s e aa =
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
        |> group aa


hsl h s l =
    Color.hsl h s l |> Color.toCssString


hsla h s l a =
    Color.hsla h s l a |> Color.toCssString
