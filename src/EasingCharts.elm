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
            , C.bars
                [ CA.x1 .x ]
                [ C.bar .z [ CA.striped [] ]
                , C.bar .y []
                ]
                [ { x = 3, y = 3, z = 1 }
                , { x = 4, y = 2, z = 3 }
                , { x = 5, y = 4, z = 2 }
                ]
            , C.series .x
                [ C.stacked
                    [ C.interpolated .y [ CA.opacity 0.2 ] []
                    , C.interpolated .z [ CA.opacity 1, CA.dotted [] ] []
                    ]
                ]
                [ { x = 1, y = 1, z = 3 }
                , { x = 2, y = 2, z = 1 }
                , { x = 3, y = 2, z = 4 }
                ]
            , C.series .x
                [ C.scatter .y [ CA.circle ]
                , C.scatter .z [ CA.square ]
                ]
                [ { x = 1, y = 2, z = 3 }
                , { x = 2, y = 3, z = 5 }
                , { x = 3, y = 4, z = 2 }
                , { x = 4, y = 1, z = 3 }
                , { x = 5, y = 4, z = 1 }
                ]
            , C.series .x
                [ C.interpolated .y [ CA.monotone ] [ CA.circle ]
                , C.interpolated .z [ CA.monotone ] [ CA.square ]
                ]
                [ { x = 1, y = 2, z = 3 }
                , { x = 3, y = 4, z = 1 }
                , { x = 5, y = 2, z = 4 }
                ]
            ]
        ]
