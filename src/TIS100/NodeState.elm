module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)


type NodeState a
    = Write Num (() -> a)
    | Done
    | Read (Num -> a)
    | Run


map : (a -> b) -> NodeState a -> NodeState b
map mapper ns =
    case ns of
        Write num resolver ->
            Write num (\() -> resolver () |> mapper)

        Done ->
            Done

        Read resolver ->
            Read (\num -> resolver num |> mapper)

        Run ->
            Run
