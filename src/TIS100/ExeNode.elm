module TIS100.ExeNode exposing (ExeNode, init, state)

import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import Utils exposing (Dir4(..))


type ExeNode
    = Done
    | ReadyToRun
    | ReadBlocked
    | WriteBlocked Num


init : ExeNode
init =
    ReadyToRun


state : ExeNode -> S.NodeState ExeNode
state node =
    case node of
        ReadyToRun ->
            S.Run (\() -> ReadBlocked)

        Done ->
            S.Done

        ReadBlocked ->
            S.Read Up WriteBlocked

        WriteBlocked num ->
            S.Write num Down (\() -> ReadyToRun)
