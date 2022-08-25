module TIS100.OutputNode exposing
    ( OutputNode
    , fromExpected
    , state
    )

import TIS100.NodeState as S exposing (NodeState)
import TIS100.Num exposing (Num)


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


state : OutputNode -> NodeState OutputNode
state node =
    case node of
        Done _ ->
            S.Done

        ReadyToRun pendingReads nums ->
            S.Run (\() -> ReadBlocked pendingReads nums)

        ReadBlocked pendingReads nums ->
            S.Read S.Up (resolveRead pendingReads nums)


resolveRead : Int -> List Num -> Num -> OutputNode
resolveRead pendingReads nums num =
    if pendingReads == 1 then
        Done (num :: nums)

    else
        ReadyToRun (pendingReads - 1) (num :: nums)
