module TIS100.In exposing
    ( In
    , fromList
    , stepState
    , toSelectionList
    )

import Pivot exposing (Pivot)
import TIS100.Num exposing (Num)
import TIS100.PuzzlePage.NodeState as NS
import TIS100.SelectionList as SelectionList exposing (SelectionList)
import Utils exposing (Dir4(..))


type In
    = Done (List Num)
    | Running (Pivot Num)
    | WriteBlocked (Pivot Num)


toSelectionList : In -> SelectionList Num
toSelectionList node =
    case node of
        Done nums ->
            SelectionList.None nums

        Running pivot ->
            SelectionList.Selected pivot

        WriteBlocked pivot ->
            SelectionList.Selected pivot


fromList : List Num -> In
fromList nums =
    case Pivot.fromList nums of
        Just p ->
            Running p

        Nothing ->
            Done []


afterWrite : Pivot Num -> In
afterWrite oldP =
    case Pivot.goR oldP of
        Just newP ->
            Running newP

        Nothing ->
            Done (Pivot.toList oldP)


stepState : In -> NS.NodeState In
stepState node =
    case node of
        Done _ ->
            NS.Idle

        Running p ->
            NS.ReadyToRun (\() -> WriteBlocked p)

        WriteBlocked p ->
            NS.WriteBlocked (Pivot.getC p) Down (\() -> afterWrite p)
