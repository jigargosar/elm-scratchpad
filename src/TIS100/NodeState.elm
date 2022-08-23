module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)


type NodeState a
    = WriteBlocked
    | Done
    | ReadBlocked (Num -> a)
    | ReadyToRun


map : (a -> b) -> NodeState a -> NodeState b
map fn ns =
    case ns of
        WriteBlocked ->
            WriteBlocked

        Done ->
            Done

        ReadBlocked resolver ->
            ReadBlocked (\num -> resolver num |> fn)

        ReadyToRun ->
            ReadyToRun
