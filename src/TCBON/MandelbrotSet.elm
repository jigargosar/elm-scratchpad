module TCBON.MandelbrotSet exposing (..)

import Float.Extra
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


boundsToRangeWithSteps : Int -> Bounds -> List ( Float, Float )
boundsToRangeWithSteps steps { min, max } =
    let
        xs =
            Float.Extra.range { start = min.x, end = max.x, steps = steps }

        ys =
            Float.Extra.range { start = min.y, end = max.y, steps = steps }
    in
    ys
        |> List.map (\y -> xs |> List.map (pairTo y))
        |> List.concat


initialBounds : Bounds
initialBounds =
    --{ min = vec -2.4 -1.4, max = vec 1.34 1.4 }
    --{ min = vec -2.2 -1.4, max = vec 0.6 1.4 }
    --boundsFromWH 0.001 0.001 |> centerBoundsAt -0.786 -0.16
    boundsFromWH 3 2 |> centerBoundsAt -0.8 0


boundsFromWH : Float -> Float -> Bounds
boundsFromWH w h =
    { min = vec (-w / 2) (-h / 2), max = vec (w / 2) (h / 2) }


centerBoundsAt : Float -> Float -> Bounds -> Bounds
centerBoundsAt x y bounds =
    let
        ( w, h ) =
            ( boundsWidth bounds, boundsHeight bounds )
    in
    { min = vec (x - (w / 2)) (y - h / 2), max = vec (x + w / 2) (y + h / 2) }


main =
    Svg.svg
        [ boundsToViewBox initialBounds
        , dBlock
        , noFill
        , noStroke
        , overflowHidden
        , style "outline" "auto blue"
        ]
        [ group
            [ fill black
            ]
            (let
                steps =
                    200

                cw =
                    boundsHeight initialBounds / (steps - 1)
             in
             boundsToRangeWithSteps 200 initialBounds
                |> List.filterMap
                    (\( a, b ) ->
                        if belongsToMSet ( a, b ) then
                            Just (square cw [ xf [ mv2 a b ] ])

                        else
                            Nothing
                    )
            )
        ]


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
