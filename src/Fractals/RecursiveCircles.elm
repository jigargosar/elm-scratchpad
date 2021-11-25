module Fractals.RecursiveCircles exposing (..)

import Svg
import Svg.Attributes as SA
import Utils exposing (..)


width =
    500


height =
    500


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
            [ genCirc 0 0 1000 []
                |> List.map (\( x, y, r ) -> circle r [ xf [ mv2 x y ] ])
                |> group []
            ]
        ]


genCirc : Float -> Float -> Float -> List ( Float, Float, Float ) -> List ( Float, Float, Float )
genCirc x y r acc =
    if r > 2 then
        genCirc x y (r * 0.75) (( x, y, r ) :: acc)

    else
        ( x, y, r ) :: acc
