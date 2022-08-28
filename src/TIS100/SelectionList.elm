module TIS100.SelectionList exposing (..)

import Pivot exposing (Pivot)


type SelectionList a
    = None (List a)
    | Selected (Pivot a)


mapToList : (a -> b) -> (a -> b) -> SelectionList a -> List b
mapToList sfn fn sList =
    case sList of
        None ls ->
            List.map fn ls

        Selected pivot ->
            Pivot.mapCS sfn fn pivot
                |> Pivot.toList


fromIndex : Int -> List a -> SelectionList a
fromIndex idx ls =
    case Pivot.fromList ls |> Maybe.andThen (Pivot.goBy idx) of
        Just p ->
            Selected p

        Nothing ->
            None ls
