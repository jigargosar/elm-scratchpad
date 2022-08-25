module TIS100.InputNode exposing
    ( InputNode
    , fromList
    , state
    )

import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import Utils exposing (Dir4(..))


type InputNode
    = Done
    | Running Num (List Num)
    | WriteBlocked Num (List Num)


fromList : List Num -> InputNode
fromList nums =
    case nums of
        f :: r ->
            Running f r

        [] ->
            Done


state : InputNode -> S.NodeState InputNode
state node =
    case node of
        Done ->
            S.Done

        Running num nums ->
            S.Run (\() -> WriteBlocked num nums)

        WriteBlocked num nums ->
            S.Write num Down (\() -> fromList nums)
