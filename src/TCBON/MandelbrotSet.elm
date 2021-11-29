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
                                toFloat x |> rangeMap ( 0, 100 ) ( -2.4, 2.4 )

                            b =
                                toFloat y |> rangeMap ( 0, 100 ) ( -2.4, 2.4 )
                        in
                        if belongsToMSet a b 20 0 0 then
                            Just (square 1 [ xf [ mv2 (toFloat x) (toFloat y) ] ])

                        else
                            Nothing
                    )
            )
        ]


belongsToMSet cx cy n ta tb =
    let
        ( nta, ntb ) =
            ( ta * ta + cx, tb * tb + cy )

        notMember =
            (nta * nta + ntb * ntb) > 4
    in
    if notMember then
        False

    else if n <= 0 then
        not notMember

    else
        belongsToMSet cx cy (n - 1) nta ntb
