module Particles.FireworkSMIL exposing (main)

import Svg
import TypedSvg.Attributes.InPx as Px
import Utils exposing (..)


main =
    svgBlock
        [ viewBoxC 300 300
        , sMaxWidth "500px"
        ]
        [ Svg.circle [ Px.r 10, fill <| hsl 1 1 0.5 ]
            [ Svg.node "a" [] []
            ]
        ]
