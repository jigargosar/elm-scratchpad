module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)


type Dir
    = Down


type NodeState a
    = Run (() -> a)
    | Read (Num -> a)
    | Write Num Dir (() -> a)
    | Done


map : (a -> b) -> NodeState a -> NodeState b
map mapper ns =
    case ns of
        Write num dir cont ->
            Write num dir (\() -> cont () |> mapper)

        Done ->
            Done

        Read resolver ->
            Read (\num -> resolver num |> mapper)

        Run resolver ->
            Run (\() -> resolver () |> mapper)
