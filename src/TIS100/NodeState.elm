module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)


type NodeState a
    = WriteBlocked Num (() -> a)
    | Done
    | ReadBlocked (Num -> a)
    | ReadyToRun


map : (a -> b) -> NodeState a -> NodeState b
map mapper ns =
    case ns of
        WriteBlocked num resolver ->
            WriteBlocked num (\() -> resolver () |> mapper)

        Done ->
            Done

        ReadBlocked resolver ->
            ReadBlocked (\num -> resolver num |> mapper)

        ReadyToRun ->
            ReadyToRun
