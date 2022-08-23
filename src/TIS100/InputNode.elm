module TIS100.InputNode exposing
    ( InputNode
    , fromList
    , read
    , run
    , state
    , step
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
            NS.ReadyToRun

        WriteBlocked _ _ ->
            NS.WriteBlocked


run : InputNode -> InputNode
run node =
    case node of
        Running n ns ->
            WriteBlocked n ns

        WriteBlocked _ _ ->
            node

        Done ->
            node


step : InputNode -> InputNode
step node =
    case node of
        Running n ns ->
            WriteBlocked n ns

        WriteBlocked _ _ ->
            node

        Done ->
            node


read : InputNode -> Maybe ( Num, InputNode )
read node =
    case node of
        Done ->
            Nothing

        Running _ _ ->
            Nothing

        WriteBlocked num nums ->
            Just ( num, fromList nums )
