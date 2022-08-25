module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)
import Utils exposing (Dir4)


type NodeState a
    = Run (() -> a)
    | Read Dir4 (Num -> a)
    | Write Num Dir4 (() -> a)
    | Done


map : (a -> b) -> NodeState a -> NodeState b
map mapper ns =
    case ns of
        Write num dir cont ->
            Write num dir (\() -> cont () |> mapper)

        Done ->
            Done

        Read dir cont ->
            Read dir (\num -> cont num |> mapper)

        Run cont ->
            Run (\() -> cont () |> mapper)
