module TIS100.OutputNode exposing
    ( OutputNode
    , fromExpected
    , getNumsRead
    , stepState
    )

import TIS100.Num exposing (Num)
import TIS100.PuzzlePage.NodeState as NS
import Utils exposing (Dir4(..))


type OutputNode
    = Done (List Num)
    | ReadyToRun Int (List Num)
    | ReadBlocked Int (List Num)


fromExpected : Int -> OutputNode
fromExpected expected =
    if expected <= 0 then
        Done []

    else
        ReadyToRun expected []


stepState : OutputNode -> NS.NodeState OutputNode
stepState node =
    case node of
        Done _ ->
            NS.Idle

        ReadyToRun pendingReads nums ->
            NS.ReadyToRun (\() -> ReadBlocked pendingReads nums)

        ReadBlocked pendingReads nums ->
            NS.ReadBlocked Up (resolveRead pendingReads nums)


resolveRead : Int -> List Num -> Num -> OutputNode
resolveRead pendingReads nums num =
    if pendingReads == 1 then
        Done (num :: nums)

    else
        ReadyToRun (pendingReads - 1) (num :: nums)


getNumsRead : OutputNode -> List Num
getNumsRead node =
    getNumsReadHelp node |> List.reverse


getNumsReadHelp : OutputNode -> List Num
getNumsReadHelp node =
    case node of
        Done nums ->
            nums

        ReadyToRun _ nums ->
            nums

        ReadBlocked _ nums ->
            nums
