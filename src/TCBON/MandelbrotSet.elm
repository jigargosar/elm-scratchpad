module TCBON.MandelbrotSet exposing (..)

import Svg
import Utils exposing (..)


initialCRI : CRI
initialCRI =
    criFromCD (vec -0.797 -0.157) 0.015


main =
    Svg.svg
        [ criToViewBox initialCRI
        , dBlock
        , noFill
        , noStroke
        , overflowHidden
        , style "outline" "auto blue"
        ]
        [ group
            [ fill gray
            ]
            (let
                xSteps =
                    500

                cw =
                    criWidth initialCRI / xSteps
             in
             criToPointsWithXStep xSteps initialCRI
                |> List.filterMap
                    (vToTuple
                        >> (\( a, b ) ->
                                if belongsToMSet ( a, b ) then
                                    Just (square cw [ xf [ mv2 a b ] ])

                                else
                                    Nothing
                           )
                    )
            )
        ]


belongsToMSet : ComplexNum -> Bool
belongsToMSet c =
    belongsToMSetHelp 80 c ( 0, 0 )


belongsToMSetHelp : Int -> ComplexNum -> ComplexNum -> Bool
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
