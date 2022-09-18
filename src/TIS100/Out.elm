module TIS100.Out exposing
    ( Out
    , fromExpected
    , getNumsRead
    , stepState
    )

import TIS100.Num exposing (Num)
import TIS100.PuzzlePage.NodeState as NS
import Utils exposing (Dir4(..))


type Out
    = Done (List Num)
    | ReadyToRun Int (List Num)
    | ReadBlocked Int (List Num)


fromExpected : Int -> Out
fromExpected expected =
    if expected <= 0 then
        Done []

    else
        ReadyToRun expected []


stepState : Out -> NS.NodeState Out
stepState node =
    case node of
        Done _ ->
            NS.Idle

        ReadyToRun pendingReads nums ->
            NS.ReadyToRun (\() -> ReadBlocked pendingReads nums)

        ReadBlocked pendingReads nums ->
            NS.ReadBlocked Up (resolveRead pendingReads nums)


resolveRead : Int -> List Num -> Num -> Out
resolveRead pendingReads nums num =
    if pendingReads == 1 then
        Done (num :: nums)

    else
        ReadyToRun (pendingReads - 1) (num :: nums)


getNumsRead : Out -> List Num
getNumsRead node =
    getNumsReadHelp node |> List.reverse


getNumsReadHelp : Out -> List Num
getNumsReadHelp node =
    case node of
        Done nums ->
            nums

        ReadyToRun _ nums ->
            nums

        ReadBlocked _ nums ->
            nums
