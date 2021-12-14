module EasingCharts exposing (..)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Svg exposing (Axis)
import Ease
import Html as H
import Svg as S
import Utils exposing (..)


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
            [ CA.width 300
            , CA.height 300
            , CA.domain
                [--\a -> { a | min = 0, max = 1 }
                 --CA.lowest -0.25 CA.orLower
                 --, CA.highest 1.25 CA.orHigher
                ]
            , CA.range
                [--\a -> { a | min = 0, max = 1 }
                ]
            , CA.padding <| TRBL 20 20 20 20
            ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , sampleFnToSeries <| Ease.reverse Ease.outCubic

            --, sampleFnToSeries <| Ease.inOutBounce
            --, sampleFnToSeries <| Ease.inOutElastic
            , sampleFnToSeries <| Ease.inOutBack
            ]
        ]


xySeries : List { x : Float, y : Float } -> C.Element { x : Float, y : Float } msg
xySeries =
    C.series .x [ C.interpolated .y [] [] ]


sampleFnToSeries : (Float -> Float) -> C.Element { x : Float, y : Float } msg
sampleFnToSeries fn =
    xySeries (sampleFn fn)


sampleFn : (Float -> Float) -> List { x : Float, y : Float }
sampleFn fn =
    normSamples 500 |> List.map (xyBy fn)


xyBy : (b -> a) -> b -> { x : b, y : a }
xyBy fn x =
    { x = x, y = fn x }
