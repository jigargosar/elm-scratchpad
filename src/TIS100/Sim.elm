module TIS100.Sim exposing
    ( Sim
    , init
    , step
    , view
    )

import Dict exposing (Dict)
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as State exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import Utils exposing (..)


type alias Addr =
    ( Int, Int )


type Node
    = InputNode InputNode
    | OutputNode OutputNode


type alias Store =
    Dict Addr Node


type alias ReadBlockedStore =
    Dict Addr ReadBlockedNode


type alias ReadBlockedNode =
    ( Node, Num -> Node )


type alias WriteBlockedStore =
    Dict Addr WriteBlockedNode


type alias WriteBlockedNode =
    ( Node, Num, () -> Node )


type alias NodeEntry =
    ( Addr, Node )


type alias Sim =
    { store : Store
    , cycle : Int
    }


initInputNode : Int -> Node
initInputNode nums =
    InputNode (InputNode.fromList (List.repeat nums Num.zero))


initOutputNode : Int -> Node
initOutputNode expected =
    OutputNode (OutputNode.fromExpected expected)


init : Sim
init =
    let
        col1 : Store
        col1 =
            [ initInputNode 3, initOutputNode 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 0))
                |> Dict.fromList

        col2 : Store
        col2 =
            [ initInputNode 1, initOutputNode 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 1))
                |> Dict.fromList

        nodeStore =
            Dict.union col1 col2
    in
    { store = nodeStore, cycle = 0 }


step : Sim -> Sim
step sim =
    { sim
        | store = stepNodes sim.store
        , cycle = sim.cycle + 1
    }


stepNodes : Store -> Store
stepNodes ns =
    classifyAllNodes ns
        |> resolveAllRunnable
        |> resolveAllReadBlocked
        |> resolveAllWriteBlocked


classifyAllNodes : Store -> Acc
classifyAllNodes ns =
    Dict.foldl classifyNode emptyAcc ns


classifyNode : Addr -> Node -> Acc -> Acc
classifyNode na node =
    case nodeState node of
        State.Write num fn ->
            addToWriteBlocked na ( node, num, fn )

        State.Done ->
            addToCompleted na node

        State.Read fn ->
            addToReadBlocked na ( node, fn )

        State.Run ->
            addToRunnable na node


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        InputNode inputNode ->
            InputNode.state inputNode |> State.map InputNode

        OutputNode outputNode ->
            OutputNode.state outputNode |> State.map OutputNode


resolveAllRunnable : Acc -> Acc
resolveAllRunnable acc =
    Dict.foldl resolveRunnable { acc | readyToRun = Dict.empty } acc.readyToRun


resolveRunnable : Addr -> Node -> Acc -> Acc
resolveRunnable addr node =
    resolveAfterRun addr (runNode node)


resolveAfterRun : Addr -> Node -> Acc -> Acc
resolveAfterRun addr node =
    case nodeState node of
        State.Read resolver ->
            addToReadBlocked addr ( node, resolver )

        _ ->
            addToCompleted addr node


runNode : Node -> Node
runNode node =
    case node of
        InputNode inputNode ->
            InputNode (InputNode.run inputNode)

        OutputNode outputNode ->
            OutputNode (OutputNode.run outputNode)


resolveAllReadBlocked : BlockedAcc a -> BlockedAcc a
resolveAllReadBlocked acc =
    Dict.foldl resolveReadBlocked { acc | readBlocked = Dict.empty } acc.readBlocked


resolveReadBlocked :
    Addr
    -> ReadBlockedNode
    -> WriteBlockedAcc a
    -> WriteBlockedAcc a
resolveReadBlocked addr ( node, resolver ) acc =
    case resolveWriteBlocked (addrUp addr) acc of
        Just ( num, acc2 ) ->
            addToCompleted addr (resolver num) acc2

        Nothing ->
            addToCompleted addr node acc


addrUp : Addr -> Addr
addrUp ( x, y ) =
    ( x, y - 1 )


resolveWriteBlocked :
    Addr
    -> WriteBlockedAcc a
    -> Maybe ( Num, WriteBlockedAcc a )
resolveWriteBlocked na acc =
    case Dict.get na acc.writeBlocked of
        Just ( _, num, resolver ) ->
            Just
                ( num
                , addToCompleted na
                    (resolver ())
                    { acc
                        | writeBlocked = Dict.remove na acc.writeBlocked
                    }
                )

        Nothing ->
            Nothing


resolveAllWriteBlocked : WriteBlockedAcc a -> Store
resolveAllWriteBlocked acc =
    Dict.foldl (\na ( n, _, _ ) -> Dict.insert na n) acc.completed acc.writeBlocked


type alias Acc =
    { readyToRun : Store
    , readBlocked : ReadBlockedStore
    , writeBlocked : WriteBlockedStore
    , completed : Store
    }


type alias BlockedAcc a =
    { a
        | readBlocked : ReadBlockedStore
        , writeBlocked : WriteBlockedStore
        , completed : Store
    }


type alias WriteBlockedAcc a =
    { a
        | writeBlocked : WriteBlockedStore
        , completed : Store
    }


emptyAcc : Acc
emptyAcc =
    Acc Dict.empty Dict.empty Dict.empty Dict.empty


addToWriteBlocked :
    Addr
    -> WriteBlockedNode
    -> { a | writeBlocked : WriteBlockedStore }
    -> { a | writeBlocked : WriteBlockedStore }
addToWriteBlocked na n acc =
    { acc | writeBlocked = Dict.insert na n acc.writeBlocked }


addToCompleted :
    Addr
    -> Node
    -> { a | completed : Store }
    -> { a | completed : Store }
addToCompleted na n acc =
    { acc | completed = Dict.insert na n acc.completed }


addToReadBlocked :
    Addr
    -> ReadBlockedNode
    -> { a | readBlocked : ReadBlockedStore }
    -> { a | readBlocked : ReadBlockedStore }
addToReadBlocked na n acc =
    { acc | readBlocked = Dict.insert na n acc.readBlocked }


addToRunnable :
    Addr
    -> Node
    -> { a | readyToRun : Store }
    -> { a | readyToRun : Store }
addToRunnable na n acc =
    { acc | readyToRun = Dict.insert na n acc.readyToRun }


view : Sim -> Html msg
view sim =
    div [ dGrid ] (Dict.toList sim.store |> List.map viewNode)


viewNode : NodeEntry -> Html msg
viewNode ( nodeAddr, node ) =
    let
        nodeInfo =
            ( nodeAddr, node )
    in
    div [ gridAreaXY nodeAddr ]
        [ text (Debug.toString node)
        , text (Debug.toString nodeInfo) |> always noView
        ]
