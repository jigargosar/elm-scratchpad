module TCBON.MandelbrotSet exposing (..)

import Svg
import TypedSvg.Attributes as TA
import Utils exposing (..)


main =
    Svg.svg
        [ TA.viewBox 0 0 100 100
        , dBlock
        , noFill
        , noStroke
        , overflowHidden
        , style "outline" "auto blue"
        ]
        [ group
            [ fill black
            ]
            (rangeWH 100 100
                |> List.filterMap
                    (\( x, y ) ->
                        let
                            a =
                                toFloat x |> rangeMap ( 0, 100 ) ( -2.5, 2.5 )

                            b =
                                toFloat y |> rangeMap ( 0, 100 ) ( -2.5, 2.5 )
                        in
                        if belongsToMSet ( a, b ) 20 ( 0, 0 ) then
                            Just (square 1 [ xf [ mv2 (toFloat x) (toFloat y) ] ])

                        else
                            Nothing
                    )
            )
        ]


belongsToMSet ( cx, cy ) n ( ta, tb ) =
    let
        ( nta, ntb ) =
            ( ta * ta + cx, tb * tb + cy )

        isDiverging =
            (nta * nta + ntb * ntb) > 4
    in
    if isDiverging then
        False

    else if n <= 0 then
        not isDiverging

    else
        belongsToMSet cx cy (n - 1) nta ntb
