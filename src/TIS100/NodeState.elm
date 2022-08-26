module TIS100.NodeState exposing (..)

import TIS100.Num exposing (Num)
import Utils exposing (Dir4)


type NodeState a
    = ReadyToRun (() -> a)
    | ReadBlocked Dir4 (Num -> a)
    | WriteBlocked Num Dir4 (() -> a)
    | Done


map : (a -> b) -> NodeState a -> NodeState b
map converter ns =
    case ns of
        WriteBlocked num dir cont ->
            WriteBlocked num dir (\() -> cont () |> converter)

        Done ->
            Done

        ReadBlocked dir cont ->
            ReadBlocked dir (cont >> converter)

        ReadyToRun cont ->
            ReadyToRun (cont >> converter)
