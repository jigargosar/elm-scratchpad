module TIS100.ExeNode exposing (ExeNode, initEmpty, initMovUpDown, initNop, ioIntents, state)

import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import Utils exposing (Dir4(..))


type ExeNode
    = ExeNode Inst State


type State
    = Done
    | ReadyToRun
    | ReadBlocked Dir4 Dir4
    | WriteBlocked Dir4 Num


type Inst
    = Mov Dir4 Dir4
    | Nop


initMovUpDown : ExeNode
initMovUpDown =
    ExeNode (Mov Up Down) ReadyToRun


initNop : ExeNode
initNop =
    ExeNode Nop ReadyToRun


initEmpty : ExeNode
initEmpty =
    ExeNode Nop Done


state : ExeNode -> S.NodeState ExeNode
state (ExeNode inst state_) =
    case state_ of
        ReadyToRun ->
            S.ReadyToRun (\() -> ExeNode inst (run inst))

        Done ->
            S.Done

        ReadBlocked f t ->
            S.ReadBlocked f (WriteBlocked t >> ExeNode inst)

        WriteBlocked t num ->
            S.WriteBlocked num t (\() -> ExeNode inst ReadyToRun)


run : Inst -> State
run inst =
    case inst of
        Mov f t ->
            ReadBlocked f t

        Nop ->
            ReadyToRun


ioIntents : ExeNode -> List IOIntent
ioIntents (ExeNode inst st) =
    if st == Done then
        []

    else
        case inst of
            Mov f t ->
                [ Read f, Write t ]

            Nop ->
                []
