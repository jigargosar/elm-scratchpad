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
                [ SA.id "self"
                , SA.attributeName "cx"

                --, SA.values "0;100"
                , SA.from "0"
                , SA.to "100"
                , SA.dur "2s"
                , SA.begin "0s;self.end+2s"
                , SA.fill "freeze"

                --, TA.repeatCount TT.RepeatIndefinite
                ]
                []
            ]
        ]
