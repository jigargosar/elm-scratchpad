module TIS100.Num exposing (Num, fromInt, range, zero)


type Num
    = Num Int


zero : Num
zero =
    fromInt 0


fromInt : Int -> Num
fromInt i =
    Num (clamp i)


clamp : Int -> Int
clamp i =
    if i > 999 then
        999

    else if i < -999 then
        -999

    else
        i


range : Int -> Int -> List Num
range lo hi =
    List.range lo hi |> List.map fromInt
