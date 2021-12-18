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
                [ SA.id "a_cx"
                , SA.attributeName "cx"
                , SA.values "0;100"
                , SA.dur "2s"
                , SA.begin "0s;a_cx.end+0.5s"
                , SA.fill "freeze"

                --, TA.repeatCount TT.RepeatIndefinite
                ]
                []
            , Svg.animate
                [ SA.id "a_op"
                , SA.attributeName "opacity"
                , SA.values "1;1;0"
                , SA.keyTimes "0;0.7;1"
                , SA.dur "2s"
                , SA.begin "0s;a_op.end+0.5s"
                , SA.fill "freeze"

                --, TA.repeatCount TT.RepeatIndefinite
                ]
                []
            ]
        ]
