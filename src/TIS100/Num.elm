module TIS100.Num exposing (Num, fromInt, range, toString, view, viewSelected, zero)

import Utils exposing (..)


type Num
    = Num Int


toString : Num -> String
toString (Num i) =
    String.fromInt i


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


viewSelected : Num -> Html msg
viewSelected n =
    div [ fg wBlack, bgc white ] [ text (toString n) ]


view : Num -> Html msg
view n =
    div [] [ text (toString n) ]
