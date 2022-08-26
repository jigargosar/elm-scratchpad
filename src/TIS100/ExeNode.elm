module TIS100.ExeNode exposing (ExeNode, init, potentialIO, state)

import TIS100.IOIntent exposing (IOIntent(..))
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
            S.ReadyToRun (\() -> ReadBlocked)

        Done ->
            S.Done

        ReadBlocked ->
            S.ReadBlocked Up WriteBlocked

        WriteBlocked num ->
            S.WriteBlocked num Down (\() -> ReadyToRun)


potentialIO : ExeNode -> List IOIntent
potentialIO _ =
    [ Read Up, Read Down, Write Up, Write Down ]
