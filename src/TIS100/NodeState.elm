module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)
import Utils exposing (Dir4)


type NodeState a
    = ReadyToRun (() -> a)
    | ReadBlocked Dir4 (Num -> a)
    | WriteBlocked Num Dir4 (() -> a)
    | Done


map : (a -> b) -> NodeState a -> NodeState b
map fn ns =
    case ns of
        WriteBlocked num dir cont ->
            WriteBlocked num dir (cont >> fn)

        Done ->
            Done

        ReadBlocked dir cont ->
            ReadBlocked dir (cont >> fn)

        ReadyToRun cont ->
            ReadyToRun (cont >> fn)
