module TIS100.InputNode exposing
    ( InputNode
    , fromList
    , numForView
    , state
    )

import TIS100.NodeState as S exposing (NodeState)
import TIS100.Num exposing (Num)


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


state : InputNode -> NodeState InputNode
state node =
    case node of
        Done ->
            S.Done

        Running num nums ->
            S.Run (\() -> WriteBlocked num nums)

        WriteBlocked num nums ->
            S.Write num (\() -> fromList nums)


numForView : InputNode -> Maybe Num
numForView node =
    case node of
        WriteBlocked num _ ->
            Just num

        _ ->
            Nothing
