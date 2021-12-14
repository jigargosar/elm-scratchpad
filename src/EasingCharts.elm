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
            , CA.domain [ \a -> { a | min = 0, max = 1 } ]
            , CA.range [ \a -> { a | min = 0, max = 1 } ]
            ]
            [ C.xLabels [ CA.withGrid ]
            , C.yLabels [ CA.withGrid ]
            , C.series .x
                [ C.interpolated .y [ CA.monotone ] []
                ]
                [ { x = 0, y = 0.2 }
                , { x = 0.1, y = 0.3 }
                , { x = 0.3, y = 0.4 }
                ]
            ]
        ]
