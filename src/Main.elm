module Main exposing (main)

import Html exposing (text)
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px


main =
    Svg.svg [ style "font-size" "20px" ]
        [ Svg.text_
            [ Px.x 10
            , Px.y 10
            ]
            [ text "hi" ]
        , drawLine ( 10, 10 ) ( 50, 50 ) "0.1"
        , drawLine ( 50, 50 ) ( 150, 10 ) "0.1"
        ]


drawLine a b o =
    Svg.polyline
        [ TA.points [ a, b ]
        , SA.stroke "black"
        , SA.opacity o
        ]
        []
