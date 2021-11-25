module Fractals.RecursiveCircles exposing (..)

import Playground exposing (..)


main =
    picture
        [ genRadii { min = 5, step = 10 } 1000 []
            |> List.map (strokeCircle 2)
            |> group
        ]


genRadii :
    { a | min : number, step : number }
    -> number
    -> List number
    -> List number
genRadii ({ min, step } as config) r ls =
    if r < min then
        ls |> List.reverse

    else
        genRadii config (r - abs step) (r :: ls)


strokeCircle : Number -> Number -> Shape
strokeCircle strokeWidth r =
    group
        [ circle black r
        , circle white (r - strokeWidth)
        ]
