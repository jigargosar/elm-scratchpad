module Fractals.RecursiveCircles2 exposing (..)

import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
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
            [ genCirc2 [ ( 0, 0, 1000 ) ] []
                |> Debug.log "c2"
                |> List.map (\( x, y, r ) -> circle r [ xf [ mv2 x y ] ])
                |> group []
            ]
        ]


genCirc2 pending acc =
    case pending of
        [] ->
            acc

        (( x, y, r ) as circleParams) :: newPending ->
            if r > 2 then
                let
                    rn =
                        r / 2
                in
                genCirc2
                    (( x + rn, y, rn ) :: ( x - rn, y, rn ) :: newPending)
                    (circleParams :: acc)

            else
                genCirc2 newPending acc
