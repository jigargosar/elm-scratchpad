module TIS100.InputNode exposing
    ( InputNode
    , fromList
    , stepState
    , toSelectionList
    )

import Pivot exposing (Pivot)
import TIS100.Num exposing (Num)
import TIS100.PuzzlePage.NodeState as NS
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


stepState : InputNode -> NS.NodeState InputNode
stepState node =
    case node of
        Done _ ->
            NS.Done

        Running p ->
            NS.ReadyToRun (\() -> WriteBlocked p)

        WriteBlocked p ->
            NS.WriteBlocked (Pivot.getC p) Down (\() -> afterWrite p)
