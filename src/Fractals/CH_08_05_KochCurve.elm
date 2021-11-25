module Fractals.CH_08_05_KochCurve exposing (..)

import Svg exposing (Svg)
import TypedSvg as TS
import TypedSvg.Attributes as TA
import Utils exposing (..)


main =
    Svg.svg
        [ style "width" "100vw"
        , style "height" "100vh"
        , dBlock
        , noFill
        , noStroke
        ]
        [ group
            [ style "transform" "translate(50%,50%)"
            , strokeW 1
            , stroke black
            ]
            [ [ ketchLineWithR 250 ]
                |> applyN 5 (List.concatMap createKochChildren)
                |> List.map drawKochLine
                |> group []
            ]
        ]


ketchLineWithR : Float -> KochLine
ketchLineWithR radius =
    KochLine (vec -radius 0) (vec radius 0)


type alias KochLine =
    { start : Vec, end : Vec }


drawKochLine : KochLine -> Svg msg
drawKochLine { start, end } =
    vPolyline [ start, end ] []


createKochChildren : KochLine -> List KochLine
createKochChildren { start, end } =
    let
        vDiff =
            vFromTo start end

        vDiffThird =
            vDiff |> vScale (1 / 3)

        a =
            start

        b =
            vAdd start vDiffThird

        c =
            vAdd b (vDiffThird |> vRotate (degrees -60))

        d =
            vAdd b vDiffThird

        e =
            end
    in
    [ KochLine a b, KochLine b c, KochLine c d, KochLine d e ]
