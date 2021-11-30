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



--boundsToViewBox : Bounds -> Attribute a
--boundsToViewBox b =
--    TA.viewBox b.min.x b.min.y (boundsWidth b) (boundsHeight b)


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



--boundsToRangeWithSteps : Int -> Bounds -> List ( Float, Float )
--boundsToRangeWithSteps steps ({ min, max } as b) =
--    let
--        xSteps : Float
--        xSteps =
--            toFloat steps
--
--        xs =
--            Float.Extra.range { start = min.x, end = max.x, steps = round xSteps }
--
--        ySteps : Float
--        ySteps =
--            xSteps / (boundsWidth b / boundsHeight b)
--
--        ys =
--            Float.Extra.range { start = min.y, end = max.y, steps = round ySteps }
--    in
--    ys
--        |> List.map (\y -> xs |> List.map (pairTo y))
--        |> List.concat
--initialBounds : Bounds
--initialBounds =
--{ min = vec -2.4 -1.4, max = vec 1.34 1.4 }
--{ min = vec -2.2 -1.4, max = vec 0.6 1.4 }
--boundsFromWH 0.00035 0.00035 |> centerBoundsAt -0.86192 -0.25289
--boundsFromWH 0.001 0.001 |> centerBoundsAt -0.786 -0.16
--boundsFromWH 3 2 |> centerBoundsAt -0.8 0
--boundsFromWH 0.1 0.1 |> centerBoundsAt -0.815 -0.157
--boundsFromWH 0.03 0.03 |> centerBoundsAt -0.815 -0.157
--boundsFromWH 0.015 0.015 |> centerBoundsAt -0.797 -0.157


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
                --boundsToRangeWithSteps steps initialBounds
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
