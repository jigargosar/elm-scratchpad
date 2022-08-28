module TIS100.InputNode exposing
    ( InputNode
    , fromList
    , state
    , toSelectionList
    )

import Pivot exposing (Pivot)
import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import TIS100.SelectionList as SelectionList exposing (SelectionList)
import Utils exposing (Dir4(..))


type InputNode
    = Done (List Num)
    | Running (Pivot Num)
    | WriteBlocked (Pivot Num)


toSelectionList : InputNode -> SelectionList Num
toSelectionList node =
    case node of
        Done nums ->
            SelectionList.None nums

        Running pivot ->
            SelectionList.Selected pivot

        WriteBlocked pivot ->
            SelectionList.Selected pivot


fromList : List Num -> InputNode
fromList nums =
    case Pivot.fromList nums of
        Just p ->
            Running p

        Nothing ->
            Done []


afterWrite : Pivot Num -> InputNode
afterWrite oldP =
    case Pivot.goR oldP of
        Just newP ->
            Running newP

        Nothing ->
            Done (Pivot.toList oldP)


state : InputNode -> S.NodeState InputNode
state node =
    case node of
        Done _ ->
            S.Done

        Running p ->
            S.ReadyToRun (\() -> WriteBlocked p)

        WriteBlocked p ->
            S.WriteBlocked (Pivot.getC p) Down (\() -> afterWrite p)
