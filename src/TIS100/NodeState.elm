module TIS100.NodeState exposing (..)


type NodeState a
    = WriteBlocked
    | Done
    | ReadBlocked
    | ReadyToRun


map : (a -> b) -> NodeState a -> NodeState b
map _ ns =
    case ns of
        WriteBlocked ->
            WriteBlocked

        Done ->
            Done

        ReadBlocked ->
            ReadBlocked

        ReadyToRun ->
            ReadyToRun
