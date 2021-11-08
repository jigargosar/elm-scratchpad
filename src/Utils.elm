module Utils exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT


saWidth =
    SA.width << String.fromFloat


saHeight =
    SA.height << String.fromFloat


fill =
    SA.fill


stroke =
    SA.stroke


fontSize =
    style "font-size"


words : List (Attribute msg) -> String -> Svg msg
words xs str =
    Svg.text_
        (SA.textAnchor "middle"
            :: SA.dominantBaseline "central"
            :: xs
        )
        [ Svg.text str ]
