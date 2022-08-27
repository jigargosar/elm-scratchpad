module TIS100.ExeNode exposing (ExeNode, initEmpty, initMovUpDown, ioIntents, state)

import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import Utils exposing (Dir4(..))


type ExeNode
    = Done
    | ReadyToRun Inst
    | ReadBlocked Inst
    | WriteBlocked Inst Num


type Inst
    = Mov Dir4 Dir4
    | Nop


initMovUpDown : ExeNode
initMovUpDown =
    ReadyToRun (Mov Up Down)


initEmpty : ExeNode
initEmpty =
    ReadyToRun Nop


state : ExeNode -> S.NodeState ExeNode
state node =
    case node of
        ReadyToRun is ->
            S.ReadyToRun (\() -> ReadBlocked is)

        Done ->
            S.Done

        ReadBlocked is ->
            S.ReadBlocked Up (WriteBlocked is)

        WriteBlocked is num ->
            S.WriteBlocked num Down (\() -> ReadyToRun is)


ioIntents : ExeNode -> List IOIntent
ioIntents _ =
    [ Read Up, Read Down, Write Up, Write Down ]
