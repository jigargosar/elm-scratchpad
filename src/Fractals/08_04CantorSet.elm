module Fractals.CantorSet exposing (..)

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
            if r < 4 then
                genCirc2 newPending acc

            else
                let
                    rn =
                        r * 0.5
                in
                genCirc2
                    (( x + r, y, rn )
                        :: ( x - r, y, rn )
                        :: ( x, y + r, rn )
                        :: ( x, y - r, rn )
                        :: newPending
                    )
                    (circleParams :: acc)
