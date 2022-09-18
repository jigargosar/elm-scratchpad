module TIS100.PuzzlePage.SimRunner exposing (step)

import Dict exposing (Dict)
import TIS100.Addr exposing (Addr)
import TIS100.Num exposing (Num)
import TIS100.PuzzlePage.NodeState exposing (NodeState(..))
import Utils as U exposing (Dir4)


type alias Model a =
    Dict Addr a


step : (a -> NodeState a) -> Model a -> Model a
step simNodeState store =
    Dict.foldl (stepNode simNodeState) (initAcc store) store
        |> resolveAllReadBlocked
        |> resolveAllWriteBlocked


stepNode : (a -> NodeState a) -> Addr -> a -> Acc a -> Acc a
stepNode simNodeState addr node =
    case simNodeState node of
        WriteBlocked num dir cont ->
            addToWriteBlocked addr node num dir cont

        Idle ->
            addToCompleted addr node

        ReadBlocked dir cont ->
            addToReadBlocked addr node dir cont

        ReadyToRun cont ->
            resolveAfterRun simNodeState addr (cont ())


resolveAfterRun : (a -> NodeState a) -> Addr -> a -> Acc a -> Acc a
resolveAfterRun simNodeState addr node =
    case simNodeState node of
        ReadBlocked dir cont ->
            addToReadBlocked addr node dir cont

        _ ->
            addToCompleted addr node


resolveAllReadBlocked : Acc a -> Acc a
resolveAllReadBlocked acc =
    Dict.foldl resolveReadBlocked { acc | readBlocked = Dict.empty } acc.readBlocked


resolveReadBlocked :
    Addr
    -> ReadBlockedNode a
    -> WriteBlockedAcc x a
    -> WriteBlockedAcc x a
resolveReadBlocked addr ( node, dir, cont ) acc =
    case readAndUnblock addr dir acc of
        Just ( num, acc2 ) ->
            addToCompleted addr (cont num) acc2

        Nothing ->
            addToCompleted addr node acc


readAndUnblock :
    Addr
    -> Dir4
    -> WriteBlockedAcc x a
    -> Maybe ( Num, WriteBlockedAcc x a )
readAndUnblock rAddr rDir acc =
    U.moveInDir4 rDir rAddr
        |> U.getEntryIn acc.writeBlocked
        |> U.maybeFilter
            (\( _, wbNode ) ->
                rDir == U.oppositeDir4 wbNode.dir
            )
        |> Maybe.map
            (\( wAddr, wbNode ) ->
                ( wbNode.num
                , completeWriteBlocked wAddr (wbNode.cont ()) acc
                )
            )


resolveAllWriteBlocked : WriteBlockedAcc x a -> Model a
resolveAllWriteBlocked acc =
    Dict.foldl (\addr { node } -> U.replaceEntry ( addr, node )) acc.completed acc.writeBlocked


type alias Acc a =
    { readBlocked : ReadBlockedStore a
    , writeBlocked : WriteBlockedStore a
    , completed : Model a
    }


type alias WriteBlockedAcc x a =
    { x
        | writeBlocked : WriteBlockedStore a
        , completed : Model a
    }


type alias ReadBlockedStore a =
    Dict Addr (ReadBlockedNode a)


type alias ReadBlockedNode a =
    ( a, Dir4, Num -> a )


type alias WriteBlockedStore a =
    Dict Addr (WriteBlockedNode a)


type alias WriteBlockedNode a =
    { node : a, num : Num, dir : Dir4, cont : () -> a }


initAcc : Model a -> Acc a
initAcc store =
    { readBlocked = Dict.empty
    , writeBlocked = Dict.empty
    , completed = store
    }


addToReadBlocked :
    Addr
    -> a
    -> Dir4
    -> (Num -> a)
    -> { x | readBlocked : ReadBlockedStore a }
    -> { x | readBlocked : ReadBlockedStore a }
addToReadBlocked addr node dir cont acc =
    { acc | readBlocked = Dict.insert addr ( node, dir, cont ) acc.readBlocked }


addToWriteBlocked :
    Addr
    -> a
    -> Num
    -> Dir4
    -> (() -> a)
    -> { x | writeBlocked : WriteBlockedStore a }
    -> { x | writeBlocked : WriteBlockedStore a }
addToWriteBlocked addr node num dir cont acc =
    let
        wbn =
            WriteBlockedNode node num dir cont
    in
    { acc | writeBlocked = Dict.insert addr wbn acc.writeBlocked }


addToCompleted :
    Addr
    -> a
    -> { x | completed : Model a }
    -> { x | completed : Model a }
addToCompleted na n acc =
    { acc | completed = U.replaceEntry ( na, n ) acc.completed }


completeWriteBlocked : Addr -> a -> WriteBlockedAcc x a -> WriteBlockedAcc x a
completeWriteBlocked addr node acc =
    { acc | writeBlocked = Dict.remove addr acc.writeBlocked }
        |> addToCompleted addr node
