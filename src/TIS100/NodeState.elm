module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)


type Dir
    = Down
    | Up


oppositeDir : Dir -> Dir
oppositeDir dir =
    case dir of
        Down ->
            Up

        Up ->
            Down


type NodeState a
    = Run (() -> a)
    | Read Dir (Num -> a)
    | Write Num Dir (() -> a)
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
