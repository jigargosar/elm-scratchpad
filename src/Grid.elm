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


indexFromGP : Int -> Int -> GPos -> Maybe Int
indexFromGP w h ( x, y ) =
    if validGP w h ( x, y ) then
        Just (w * y + x)

    else
        Nothing


validGP : number -> number -> ( number, number ) -> Bool
validGP w h ( x, y ) =
    x >= 0 && x < w && y >= 0 && y < h


get : GPos -> Grid a -> Maybe a
get gp grid =
    indexFromGP grid.w grid.h gp
        |> Maybe.andThen (\i -> Array.get i grid.a)


set : GPos -> a -> Grid a -> Grid a
set gp v grid =
    indexFromGP grid.w grid.h gp
        |> Maybe.map (\i -> { grid | a = Array.set i v grid.a })
        |> Maybe.withDefault grid


swap : GPos -> GPos -> Grid a -> Maybe (Grid a)
swap a b grid =
    let
        swapWithValues va vb =
            grid
                |> set a vb
                |> set b va
    in
    Maybe.map2 swapWithValues
        (get a grid)
        (get b grid)


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
