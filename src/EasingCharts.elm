module EasingCharts exposing (..)

import Chart as C
import Chart.Attributes as CA
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


main =
    div [ pa <| fpx 50 ]
        [ C.chart
            []
            [ C.xLabels []
            , C.yLabels [ CA.withGrid ]
            , C.series .x
                [ C.interpolated .y [ CA.monotone ] [ CA.circle ]
                , C.interpolated .z [ CA.monotone ] [ CA.square ]
                ]
                [ { x = 1, y = 2, z = 3 }
                , { x = 5, y = 4, z = 1 }
                , { x = 10, y = 2, z = 4 }
                ]
            ]
        ]
