module TIS100.OutputNode exposing
    ( OutputNode
    , fromExpected
    , run
    , state
    )

import TIS100.NodeState as NS exposing (NodeState)
import TIS100.Num exposing (Num)


type OutputNode
    = Done (List Num)
    | ReadyToRead Int (List Num)
    | ReadBlocked Int (List Num)


fromExpected : Int -> OutputNode
fromExpected expected =
    if expected <= 0 then
        Done []

    else
        ReadyToRead expected []


type alias ReadFn a =
    () -> Maybe ( Num, a )


state : OutputNode -> NodeState OutputNode
state node =
    case node of
        Done _ ->
            NS.Done

        ReadyToRead _ _ ->
            NS.Run

        ReadBlocked pendingReads nums ->
            resolveRead pendingReads nums |> NS.Read


resolveRead : Int -> List Num -> Num -> OutputNode
resolveRead pendingReads nums num =
    if pendingReads == 1 then
        Done (num :: nums)

    else
        ReadyToRead (pendingReads - 1) (num :: nums)


run : OutputNode -> OutputNode
run node =
    case node of
        Done _ ->
            node

        ReadyToRead pendingReads nums ->
            ReadBlocked pendingReads nums

        ReadBlocked _ _ ->
            node
