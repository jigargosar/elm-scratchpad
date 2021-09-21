module Main exposing (main)

import Html exposing (text)
import Html.Attributes exposing (style)
import Svg
import TypedSvg.Attributes.InPx as Px


main =
    Svg.svg [ style "font-size" "20px" ]
        [ Svg.text_
            [ Px.x 10
            , Px.y 10
            ]
            [ text "hi" ]
        ]
