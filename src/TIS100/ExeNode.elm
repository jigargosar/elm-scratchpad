module TIS100.ExeNode exposing (ExeNode, initEmpty, initMovUpDown, ioIntents, state)

import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import Utils exposing (Dir4(..))


type ExeNode
    = ExeNode Inst State


type State
    = Done
    | ReadyToRun
    | ReadBlocked
    | WriteBlocked Num


type Inst
    = Mov Dir4 Dir4
    | Nop


initMovUpDown : ExeNode
initMovUpDown =
    ExeNode (Mov Up Down) ReadyToRun


initEmpty : ExeNode
initEmpty =
    ExeNode Nop ReadyToRun


state : ExeNode -> S.NodeState ExeNode
state (ExeNode inst state_) =
    case state_ of
        ReadyToRun ->
            S.ReadyToRun (\() -> ExeNode inst ReadBlocked)

        Done ->
            S.Done

        ReadBlocked ->
            S.ReadBlocked Up (WriteBlocked >> ExeNode inst)

        WriteBlocked num ->
            S.WriteBlocked num Down (\() -> ExeNode inst ReadyToRun)


ioIntents : ExeNode -> List IOIntent
ioIntents _ =
    [ Read Up, Read Down, Write Up, Write Down ]
