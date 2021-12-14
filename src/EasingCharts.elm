module EasingCharts exposing (..)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Svg as CS exposing (Axis)
import Html as H
import Svg as S
import Utils exposing (..)


type alias TRBL =
    { bottom : Float
    , top : Float
    , right : Float
    , left : Float
    }


trblAll n =
    TRBL n n n n


cpa n =
    CA.padding <| trblAll n


cma n =
    CA.margin <| trblAll n


type alias Container data msg =
    { width : Float
    , height : Float
    , margin : { top : Float, bottom : Float, left : Float, right : Float }
    , padding : { top : Float, bottom : Float, left : Float, right : Float }
    , responsive : Bool
    , range : List (CA.Attribute Axis)
    , domain : List (CA.Attribute Axis)
    , events : List (CE.Event data msg)
    , htmlAttrs : List (H.Attribute msg)
    , attrs : List (S.Attribute msg)
    }


chart : List (CA.Attribute (Container data msg)) -> List (C.Element data msg) -> H.Html msg
chart =
    C.chart


main =
    div [ paf 50 ]
        [ chart
            [ CA.height 300
            , CA.width 300
            , CA.domain [ \a -> { a | min = 0, max = 1 } ]
            , CA.range [ \a -> { a | min = 0, max = 1 } ]
            ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , C.series .x
                [ C.interpolated .y [ CA.monotone ] [] ]
                (sampleFn sqr)
            ]
        ]


sqr x =
    x ^ 2


sampleFn : (Float -> a) -> List { x : Float, y : a }
sampleFn fn =
    normSamples 100
        |> List.map (\x -> { x = x, y = fn x })
