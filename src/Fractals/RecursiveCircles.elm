module Fractals.RecursiveCircles exposing (..)

import Playground exposing (..)


main =
    picture
        [ genRadii { min = 5, step = 10 } 1000 []
            |> List.map (strokeCircle 2)
            |> group
        , radii
            |> List.reverse
            |> Debug.log "radii"
            |> List.map (strokeCircle 2)
            |> group
            |> fade 0.5
        , radii2
            |> Debug.log "radii"
            |> List.map (strokeCircle 2)
            |> group
            |> fade 0.5
        ]


radii2 =
    iterate
        (\r ->
            if r > 1000 then
                Nothing

            else
                Just (r * 1.75)
        )
        5
        []


radii =
    iterate
        (\r ->
            if r < 5 then
                Nothing

            else
                Just (r * 0.75)
        )
        1000
        []


iterate : (a -> Maybe a) -> a -> List a -> List a
iterate mbFn root xs =
    case mbFn root of
        Nothing ->
            xs

        Just nextRoot ->
            iterate mbFn nextRoot (root :: xs)


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
