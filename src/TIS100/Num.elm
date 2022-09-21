module TIS100.Num exposing
    ( Num
    , add
    , fromInt
    , isGreaterThanZero
    , parse
    , range
    , toString
    , view
    , viewError
    , viewSelectionList
    , zero
    )

import TIS100.SelectionList as SelectionList exposing (SelectionList)
import TIS100.UI as UI
import Utils exposing (..)


type Num
    = Num Int


fromInt : Int -> Num
fromInt i =
    Num (clamp i)


parse : String -> Maybe Num
parse string =
    String.toInt string |> Maybe.map fromInt


isGreaterThanZero : Num -> Bool
isGreaterThanZero (Num i) =
    i > 0


zero : Num
zero =
    fromInt 0


add : Num -> Num -> Num
add (Num a) (Num b) =
    fromInt (a + b)


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


toString : Num -> String
toString (Num i) =
    String.fromInt i


view : Num -> Html msg
view n =
    div [] [ text (toString n) ]


viewSelected : Num -> Html msg
viewSelected num =
    div [ fg wBlack, bgc white ] [ text (toString num) ]


viewSelectionList : SelectionList Num -> List (Html msg)
viewSelectionList =
    SelectionList.view viewSelected view


viewError : Num -> Html msg
viewError num =
    div [ fg wBlack, bgc UI.errorRed ] [ text (toString num) ]
