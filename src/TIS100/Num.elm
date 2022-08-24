module TIS100.Num exposing (Num, range, zero)


type Num
    = Num Int


zero : Num
zero =
    Num 0


range : Int -> Int -> List Num
range lo hi =
    List.range lo hi |> List.map Num
