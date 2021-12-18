module Particles.FireworkSMIL exposing (main)

import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT
import Utils exposing (..)


main =
    svgBlock
        [ viewBoxC 300 300
        , sMaxWidth "500px"
        ]
        [ Svg.circle [ Px.r 10, fill <| hsl 1 1 0.5 ]
            [ Svg.animate
                [ SA.attributeName "cx"
                , TA.from 0
                , TA.to 100
                , TA.dur <| TT.Duration "2s"
                , TA.repeatCount TT.RepeatIndefinite
                ]
                []
            ]
        ]
