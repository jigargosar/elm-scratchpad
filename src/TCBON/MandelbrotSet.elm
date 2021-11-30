module TCBON.MandelbrotSet exposing (..)

import Float.Extra
import Html exposing (Attribute)
import Svg
import TypedSvg.Attributes as TA
import Utils exposing (..)


type alias CRI =
    { c : Vec, ri : Vec }


newCRI : Vec -> Vec -> CRI
newCRI c ri =
    { c = c, ri = ri }


criFromCR : Vec -> Float -> CRI
criFromCR c r =
    newCRI c (vec r r)


criFromCD : Vec -> Float -> CRI
criFromCD c d =
    criFromCR c (d / 2)


criToBounds : CRI -> Bounds
criToBounds cri =
    { min = criToMin cri, max = criToMax cri }


criAspectRatio : CRI -> Float
criAspectRatio =
    criToWH >> (\( w, h ) -> w / h)


criToWH : CRI -> Float2
criToWH cri =
    ( criWidth cri, criHeight cri )


criWidth =
    .ri >> .x >> mul 2


criHeight =
    .ri >> .y >> mul 2


criToMin : CRI -> Vec
criToMin { c, ri } =
    vAdd c (vNegate ri)


criToMax : CRI -> Vec
criToMax { c, ri } =
    vAdd c ri


criToViewBox : CRI -> Attribute a
criToViewBox cri =
    let
        { x, y } =
            criToMin cri

        ( w, h ) =
            criToWH cri
    in
    TA.viewBox x y w h


type alias Bounds =
    { min : Vec, max : Vec }


criToPointsWithXStep : Int -> CRI -> List Vec
criToPointsWithXStep intXSteps cri =
    let
        xSteps : Float
        xSteps =
            toFloat intXSteps

        { min, max } =
            criToBounds cri

        xs =
            Float.Extra.range { start = min.x, end = max.x, steps = round xSteps }

        ySteps : Float
        ySteps =
            xSteps / criAspectRatio cri

        ys =
            Float.Extra.range { start = min.y, end = max.y, steps = round ySteps }
    in
    ys
        |> List.map (\y -> xs |> List.map (\x -> vec x y))
        |> List.concat


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
