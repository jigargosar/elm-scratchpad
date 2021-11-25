module Fractals.CH_08_02_RecursiveCircles2X exposing (..)

import Svg
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
            [ genCirc2 [ ( 0, 0, 200 ) ] []
                |> List.map (\( x, y, r ) -> circle r [ xf [ mv2 x y ] ])
                |> group []
            ]
        ]


genCirc2 pending acc =
    case pending of
        [] ->
            acc

        (( x, y, r ) as circleParams) :: newPending ->
            if r < 2 then
                genCirc2 newPending acc

            else
                let
                    newR =
                        r / 2

                    xOff =
                        newR * 2
                in
                genCirc2
                    (( x + xOff, y, newR )
                        :: ( x - xOff, y, newR )
                        :: newPending
                    )
                    (circleParams :: acc)
