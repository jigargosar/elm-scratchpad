module TCBON.MandelbrotSet exposing (..)

import Svg
import TypedSvg.Attributes as TA
import Utils exposing (..)


main =
    Svg.svg
        [ TA.viewBox
            0
            0
            100
            100
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
                |> List.filter
                    (\( x, y ) ->
                        let
                            a =
                                toFloat x |> rangeMap ( 0, 100 ) ( -2, 2 )

                            b =
                                toFloat y |> rangeMap ( 0, 100 ) ( -2, 2 )
                        in
                        iterate a b 50 0 0
                    )
                |> List.map (vFromIntTuple >> (\p -> square 1 [ xf [ mv p ] ]))
            )
        ]


iterate cx cy n a b =
    let
        isLessThan2 =
            sqrt (a ^ 2 + b ^ 2) < 2
    in
    if n <= 0 then
        isLessThan2

    else if isLessThan2 then
        iterate cx cy (n - 1) (cx + a ^ 2) (cy + b ^ 2)

    else
        False
