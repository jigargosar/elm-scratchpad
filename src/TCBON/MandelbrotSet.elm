module TCBON.MandelbrotSet exposing (..)

import Svg
import TypedSvg.Attributes as TA
import Utils exposing (..)


main =
    Svg.svg
        [ TA.viewBox
            0
            0
            100
            100
        , dBlock
        , noFill
        , noStroke
        , overflowHidden
        , style "outline" "auto blue"
        ]
        [ group
            [ fill black
            ]
            (rangeWH 100 100
                |> List.map (vFromIntTuple >> (\p -> square 1 [ xf [ mv p ] ]))
            )
        ]
