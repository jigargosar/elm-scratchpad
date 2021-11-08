module Utils exposing (..)

import Browser
import Color
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT


type alias GPos =
    ( Int, Int )


adjacentGPSInGridOf : Int -> Int -> GPos -> List GPos
adjacentGPSInGridOf w h gp =
    let
        adjacentOffsets =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]

        gpAdd : GPos -> GPos -> GPos
        gpAdd ( a, b ) ( c, d ) =
            ( a + c, b + d )

        validateGP m =
            rangeWH w h |> List.member m
    in
    adjacentOffsets
        |> List.map (gpAdd gp)
        |> List.filter validateGP


areAdjacent : GPos -> GPos -> Bool
areAdjacent gp1 gp2 =
    let
        adjacentOffsets =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]

        gpAdd : GPos -> GPos -> GPos
        gpAdd ( a, b ) ( c, d ) =
            ( a + c, b + d )
    in
    adjacentOffsets
        |> List.map (gpAdd gp1)
        |> List.member gp2


rangeWH : Int -> Int -> List GPos
rangeWH w h =
    rangeN h
        |> List.concatMap (\y -> rangeN w |> List.map (\x -> ( x, y )))


rangeN : Int -> List Int
rangeN n =
    List.range 0 (n - 1)


type alias Vec =
    { x : Float, y : Float }


vFromGP : GPos -> Vec
vFromGP ( gx, gy ) =
    vec (toFloat gx) (toFloat gy)


vec : Float -> Float -> Vec
vec =
    Vec


vScale : Float -> Vec -> Vec
vScale s { x, y } =
    vec (s * x) (s * y)


vSub1 : Float -> Vec -> Vec
vSub1 a { x, y } =
    vec (x - a) (y - a)


vAdd1 : Float -> Vec -> Vec
vAdd1 a { x, y } =
    vec (x + a) (y + a)


group =
    Svg.g


xf =
    TA.transform


mv2 =
    TT.Translate


scale s =
    TT.Scale s s


mv { x, y } =
    mv2 x y


black =
    grayN 0.1


gray =
    grayN 0.2


white =
    grayN 1


grayN n =
    rgb n n n


rgb r g b =
    Color.rgb r g b |> Color.toCssString


bgc =
    style "background-color"


noFill =
    fill "none"


noStroke =
    stroke "none"


saWidth =
    SA.width << String.fromFloat


saHeight =
    SA.height << String.fromFloat


fill =
    SA.fill


stroke =
    SA.stroke


fontSize =
    style "font-size"


noUserSelect =
    style "user-select" "none"


words : List (Attribute msg) -> String -> Svg msg
words xs str =
    Svg.text_
        (SA.textAnchor "middle"
            :: SA.dominantBaseline "central"
            :: xs
        )
        [ Svg.text str ]
