module Grid exposing (Grid, foldValues, get, init, initIndexed, set, swap, toList)

import Array exposing (Array)
import Utils exposing (..)


type alias Grid a =
    { w : Int, h : Int, a : Array a }


init : Int -> Int -> (GPos -> a) -> Grid a
init w h fn =
    { w = w
    , h = h
    , a = Array.initialize (w * h) (\i -> fn (indexToGP w i))
    }


initIndexed : Int -> Int -> (Int -> GPos -> a) -> Grid a
initIndexed w h fn =
    { w = w
    , h = h
    , a = Array.initialize (w * h) (\i -> fn i (indexToGP w i))
    }


indexToGP : Int -> Int -> GPos
indexToGP w i =
    ( modBy w i, i // w )


indexFromGP : Int -> GPos -> Int
indexFromGP w ( x, y ) =
    w * y + x


get : GPos -> Grid a -> Maybe a
get gp grid =
    let
        i =
            indexFromGP grid.w gp
    in
    Array.get i grid.a


set : GPos -> a -> Grid a -> Grid a
set gp v grid =
    let
        i =
            indexFromGP grid.w gp
    in
    { grid | a = Array.set i v grid.a }


swap : GPos -> GPos -> Grid a -> Maybe (Grid a)
swap a b grid =
    let
        ia =
            indexFromGP grid.w a

        ib =
            indexFromGP grid.w b

        swapWithValues va vb =
            { grid
                | a =
                    grid.a
                        |> Array.set ia vb
                        |> Array.set ib va
            }
    in
    Maybe.map2 swapWithValues
        (Array.get ia grid.a)
        (Array.get ib grid.a)


toArray_ : Grid a -> Array a
toArray_ =
    .a


foldValues : (a -> b -> b) -> b -> Grid a -> b
foldValues fn acc grid =
    toArray_ grid
        |> Array.foldl fn acc


toList : Grid a -> List ( GPos, a )
toList grid =
    grid.a
        |> Array.toIndexedList
        |> List.map (mapFirst (indexToGP grid.w))
