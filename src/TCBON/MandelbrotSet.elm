module TCBON.MandelbrotSet exposing (..)

import Html exposing (Attribute, Html, div)
import Svg exposing (Svg)
import TypedSvg.Attributes as TA
import Utils exposing (..)


xSteps =
    250


maxT =
    80


initialCri : CRI
initialCri =
    --{ min = vec -2.4 -1.4, max = vec 1.34 1.4 }
    --{ min = vec -2.2 -1.4, max = vec 0.6 1.4 }
    --boundsFromWH 0.00035 0.00035 |> centerBoundsAt -0.86192 -0.25289
    --boundsFromWH 0.001 0.001 |> centerBoundsAt -0.786 -0.16
    --boundsFromWH 3 2 |> centerBoundsAt -0.8 0
    --boundsFromWH 0.1 0.1 |> centerBoundsAt -0.815 -0.157
    --boundsFromWH 0.03 0.03 |> centerBoundsAt -0.815 -0.157
    --boundsFromWH 0.015 0.015 |> centerBoundsAt -0.797 -0.157
    criFromCD (vec -0.797 -0.157) 0.015


type alias Mandel =
    { resolution : Int
    , maxT : Int
    , xRange : Float2
    , yRange : Float2
    }


initialMandel : Mandel
initialMandel =
    let
        ( xRange, yRange ) =
            criFromCD (vec -0.797 -0.157) 0.015
                |> criToXYRanges
    in
    { resolution = 250, maxT = 80, xRange = xRange, yRange = yRange }


mandelRender : Mandel -> Html msg
mandelRender mandel =
    let
        inputRange : Float2
        inputRange =
            ( 0, toFloat mandel.resolution )

        i2ToComplex : Int2 -> ComplexNum
        i2ToComplex =
            toFloat2
                >> mapBoth (rangeMap inputRange mandel.xRange)
                    (rangeMap inputRange mandel.yRange)

        renderInt2 : Int2 -> Maybe (Svg msg)
        renderInt2 i2 =
            if belongsToMSet mandel.maxT (i2ToComplex i2) then
                Just (square 0.5 [ xf [ mvInt2 i2 ] ])

            else
                Nothing

        mandelViewBox : Attribute a
        mandelViewBox =
            let
                w =
                    toFloat mandel.resolution
            in
            TA.viewBox 0 0 w w
    in
    rangeWH mandel.resolution mandel.resolution
        |> List.filterMap renderInt2
        |> Svg.svg
            [ mandelViewBox
            , dBlock
            , noFill
            , noStroke
            , overflowHidden
            , style "outline" "auto blue"
            ]


main =
    let
        cri : CRI
        cri =
            initialCri

        --( w, h ) =
        --    ( xSteps, xSteps )
    in
    div []
        [ Svg.svg
            [ criToViewBox cri

            --, saWidth w
            --, saHeight h
            , dBlock
            , noFill
            , noStroke
            , overflowHidden
            , style "outline" "auto blue"
            ]
            [ renderMPoints cri ]
        , mandelRender initialMandel
        ]


renderMPoints : CRI -> Svg msg
renderMPoints cri =
    let
        cw =
            criWidth cri / xSteps

        renderMaybe c =
            if belongsToMSet maxT c then
                Just (renderPt cw c)

            else
                Nothing
    in
    group [ fill gray ]
        (criToPointsWithXStep xSteps cri
            |> List.filterMap renderMaybe
        )


renderPt : Float -> ComplexNum -> Svg msg
renderPt len ( x, y ) =
    square len [ xf [ mv2 x y ] ]


belongsToMSet : Int -> ComplexNum -> Bool
belongsToMSet maxT_ c =
    let
        t0 =
            ( 0, 0 )
    in
    belongsToMSetHelp maxT_ c t0


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
    Float2


complexSquare : ComplexNum -> ComplexNum
complexSquare ( a, b ) =
    ( a ^ 2 - b ^ 2, 2 * a * b )


complexAdd : ComplexNum -> ComplexNum -> ComplexNum
complexAdd ( a, b ) ( c, d ) =
    ( a + c, b + d )


complexLengthSquared : ComplexNum -> Float
complexLengthSquared ( a, b ) =
    a ^ 2 + b ^ 2
