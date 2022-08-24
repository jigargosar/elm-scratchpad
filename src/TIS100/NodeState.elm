module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)


type NodeState a
    = Run (() -> a)
    | Read (Num -> a)
    | Write Num (() -> a)
    | Done


map : (a -> b) -> NodeState a -> NodeState b
map mapper ns =
    case ns of
        Write num resolver ->
            Write num (\() -> resolver () |> mapper)

        Done ->
            Done

        Read resolver ->
            Read (\num -> resolver num |> mapper)

        Run resolver ->
            Run (\() -> resolver () |> mapper)
