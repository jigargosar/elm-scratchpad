module TIS100.OutputNode exposing
    ( OutputNode
    , fromExpected
    , getNumsRead
    , state
    )

import TIS100.NodeState as S
import TIS100.Num exposing (Num)
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


state : OutputNode -> S.NodeState OutputNode
state node =
    case node of
        Done _ ->
            S.Done

        ReadyToRun pendingReads nums ->
            S.ReadyToRun (\() -> ReadBlocked pendingReads nums)

        ReadBlocked pendingReads nums ->
            S.ReadBlocked Up (resolveRead pendingReads nums)


resolveRead : Int -> List Num -> Num -> OutputNode
resolveRead pendingReads nums num =
    if pendingReads == 1 then
        Done (num :: nums)

    else
        ReadyToRun (pendingReads - 1) (num :: nums)


getNumsRead : OutputNode -> List Num
getNumsRead node =
    case node of
        Done nums ->
            nums

        ReadyToRun _ nums ->
            nums

        ReadBlocked _ nums ->
            nums
