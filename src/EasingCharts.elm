module EasingCharts exposing (..)

import Chart as C
import Chart.Attributes as CA


type alias TRBL =
    { bottom : Float
    , top : Float
    , right : Float
    , left : Float
    }


main =
    C.chart
        [ CA.height 300
        , CA.width 300
        , CA.padding { top = 10, bottom = 10, left = 10, right = 10 }
        ]
        [ C.xLabels [ CA.withGrid, CA.ints ]
        , C.yLabels [ CA.withGrid ]
        , C.bars
            [ CA.x1 .x ]
            [ C.bar .z [ CA.striped [] ]
            , C.bar .y []
            ]
            [ { x = 1, y = 3, z = 1 }
            , { x = 2, y = 2, z = 3 }
            , { x = 3, y = 4, z = 2 }
            ]
        ]
