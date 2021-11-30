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
                        if belongsToMSet ( a, b ) then
                            Just (square 1 [ xf [ mv2 (toFloat x) (toFloat y) ] ])

                        else
                            Nothing
                    )
            )
        ]


type alias ComplexNum =
    ( Float, Float )


complexSquare : ComplexNum -> ComplexNum
complexSquare ( a, b ) =
    ( a ^ 2 - b ^ 2, 2 * a * b )


complexAdd : ComplexNum -> ComplexNum -> ComplexNum
complexAdd ( a, b ) ( c, d ) =
    ( a + c, b + d )


complexLengthSquared : ComplexNum -> Float
complexLengthSquared ( a, b ) =
    a ^ 2 + b ^ 2


belongsToMSet : ComplexNum -> Bool
belongsToMSet c =
    belongsToMSetHelp 20 c ( 0, 0 )


belongsToMSetHelp n c t0 =
    let
        t1 =
            complexSquare t0 |> complexAdd c

        isDiverging =
            complexLengthSquared t1 > 4
    in
    if isDiverging then
        False

    else if n <= 0 then
        not isDiverging

    else
        belongsToMSetHelp (n - 1) c t1
