module Grid exposing (..)

import Array exposing (Array)
import Utils exposing (..)


type alias Grid a =
    { w : Int, h : Int, a : Array a }


init : Int -> Int -> (GPos -> a) -> Grid a
init w h fn =
    Grid w h (Array.initialize (w * h) (\i -> fn (toGP w i)))


toGP : Int -> Int -> GPos
toGP w i =
    ( modBy w i, i // w - 1 )


gpToI : Int -> GPos -> Int
gpToI w ( x, y ) =
    w * y + x


get : GPos -> Grid a -> Maybe a
get gp grid =
    let
        i =
            gpToI grid.w gp
    in
    Array.get i grid.a


set : GPos -> a -> Grid a -> Grid a
set gp v grid =
    let
        i =
            gpToI grid.w gp
    in
    { grid | a = Array.set i v grid.a }


swap : GPos -> GPos -> Grid a -> Maybe (Grid a)
swap a b grid =
    let
        ia =
            gpToI grid.w a

        ib =
            gpToI grid.w b

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
