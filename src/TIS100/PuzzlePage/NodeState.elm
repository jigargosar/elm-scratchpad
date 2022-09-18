module TIS100.PuzzlePage.NodeState exposing (NodeState(..), map)

import TIS100.Num exposing (Num)
import Utils exposing (Dir4)


type NodeState a
    = ReadyToRun (() -> a)
    | ReadBlocked Dir4 (Num -> a)
    | WriteBlocked Num Dir4 (() -> a)
    | Idle


map : (a -> b) -> NodeState a -> NodeState b
map fn nodeState =
    case nodeState of
        WriteBlocked num dir cont ->
            WriteBlocked num dir (cont >> fn)

        Idle ->
            Idle

        ReadBlocked dir cont ->
            ReadBlocked dir (cont >> fn)

        ReadyToRun cont ->
            ReadyToRun (cont >> fn)
