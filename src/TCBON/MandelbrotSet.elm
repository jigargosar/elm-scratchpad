module TCBON.MandelbrotSet exposing (..)

import Html exposing (Attribute)
import Svg
import TypedSvg.Attributes as TA
import Utils exposing (..)


type alias Bounds =
    { min : Vec, max : Vec }


boundsWidth : Bounds -> Float
boundsWidth { min, max } =
    max.x - min.x


boundsHeight : Bounds -> Float
boundsHeight { min, max } =
    max.y - min.y


boundsToViewBox : Bounds -> Attribute a
boundsToViewBox b =
    TA.viewBox b.min.x b.min.y (boundsWidth b) (boundsHeight b)


initialBounds : Bounds
initialBounds =
    { min = vec -2.5 -2.5, max = vec 2.5 2.5 }


main =
    Svg.svg
        [ TA.viewBox 0 0 100 100
        , boundsToViewBox initialBounds
        , dBlock
        , noFill
        , noStroke
        , overflowHidden
        , style "outline" "auto blue"
        ]
        [ group
            [ fill black

            --, noFill
            --, strokeW 1
            --, stroke black
            ]
            (rangeWH 100 100
                |> List.filterMap
                    (mapEach toFloat
                        >> (\( x, y ) ->
                                let
                                    a =
                                        x |> rangeMap ( 0, 100 ) ( -2.5, 2.5 )

                                    b =
                                        y |> rangeMap ( 0, 100 ) ( -2.5, 2.5 )
                                in
                                if belongsToMSet ( a, b ) then
                                    Just (square 0.01 [ xf [ mv2 a b ] ])

                                else
                                    Nothing
                           )
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
