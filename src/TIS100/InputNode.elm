module TIS100.InputNode exposing
    ( InputNode
    , fromList
    , state
    )

import Pivot exposing (Pivot)
import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import Utils exposing (Dir4(..))


type InputNode
    = Done (List Num)
    | Running (Pivot Num)
    | WriteBlocked (Pivot Num)


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
