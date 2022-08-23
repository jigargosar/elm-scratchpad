module TIS100.NodeState exposing (..)


type NodeState
    = WriteBlocked
    | Done
    | ReadBlocked
    | ReadyToRun
