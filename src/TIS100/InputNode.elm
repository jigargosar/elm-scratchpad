module TIS100.InputNode exposing
    ( InputNode
    , fromList
    , run
    , state
    )

import TIS100.NodeState as NS exposing (NodeState)
import TIS100.Num exposing (Num)


type InputNode
    = Done
    | Running Num (List Num)
    | WriteBlocked Num (List Num)


fromList : List Num -> InputNode
fromList nums =
    case nums of
        f :: r ->
            Running f r

        [] ->
            Done


state : InputNode -> NodeState InputNode
state node =
    case node of
        Done ->
            NS.Done

        Running _ _ ->
            NS.Run

        WriteBlocked num nums ->
            NS.Write num (\() -> fromList nums)


run : InputNode -> InputNode
run node =
    case node of
        Running n ns ->
            WriteBlocked n ns

        WriteBlocked _ _ ->
            node

        Done ->
            node
