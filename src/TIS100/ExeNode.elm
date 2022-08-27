module TIS100.ExeNode exposing (ExeNode, initEmpty, initMovUpDown, ioIntents, state)

import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import Utils exposing (Dir4(..))


type ExeNode
    = Done
    | ReadyToRun
    | ReadBlocked
    | WriteBlocked Num


type Inst
    = Mov Dir4 Dir4
    | Nop


initMovUpDown : ExeNode
initMovUpDown =
    ReadyToRun


initEmpty : ExeNode
initEmpty =
    Debug.todo "todo"


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


ioIntents : ExeNode -> List IOIntent
ioIntents _ =
    [ Read Up, Read Down, Write Up, Write Down ]
