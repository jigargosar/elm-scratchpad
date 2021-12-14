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
    div [ paf 50 ]
        [ C.chart
            [ CA.height 300
            , CA.width 300
            , CA.padding { top = 0, bottom = 5, left = 10, right = 10 }
            ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , C.series .x
                [ C.interpolated .y [ CA.monotone ] []
                , C.interpolated .z [ CA.monotone ] []
                ]
                [ { x = 1, y = 2, z = 3 }
                , { x = 3, y = 4, z = 1 }
                , { x = 5, y = 2, z = 4 }
                ]
            ]
        ]
