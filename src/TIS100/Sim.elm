module TIS100.Sim exposing
    ( Sim
    , init
    , step
    , view
    )

import Dict exposing (Dict)
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as NS exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import Utils exposing (..)


type alias NodeAddr =
    ( Int, Int )


type Node
    = InputNode InputNode
    | OutputNode OutputNode


type alias NodeStore =
    Dict NodeAddr Node


type alias ReadBlockedStore =
    Dict NodeAddr ReadBlockedNode


type alias ReadBlockedNode =
    ( Node, Num -> Node )


type alias WriteBlockedNode =
    ( Node, Num, () -> Node )


type alias WriteBlockedStore =
    Dict NodeAddr WriteBlockedNode


type alias NodeEntry =
    ( NodeAddr, Node )


type alias Sim =
    { nodeStore : NodeStore
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
        col1 : NodeStore
        col1 =
            [ initInputNode 3, initOutputNode 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 0))
                |> Dict.fromList

        col2 : NodeStore
        col2 =
            [ initInputNode 1, initOutputNode 3 ]
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 1))
                |> Dict.fromList

        nodeStore =
            Dict.union col1 col2
    in
    { nodeStore = nodeStore, cycle = 0 }


step : Sim -> Sim
step sim =
    { sim
        | nodeStore = stepNodes sim.nodeStore
        , cycle = sim.cycle + 1
    }


stepNodes : NodeStore -> NodeStore
stepNodes ns =
    classifyNodes ns
        |> resolveAllRunnable
        |> resolveAllReadBlocked
        |> resolveAllWriteBlocked


classifyNodes : NodeStore -> Acc
classifyNodes ns =
    let
        classifyNode : NodeAddr -> Node -> Acc -> Acc
        classifyNode na node =
            case nodeState node of
                NS.WriteBlocked num fn ->
                    addToWriteBlocked na ( node, num, fn )

                NS.Done ->
                    addToCompleted na node

                NS.ReadBlocked fn ->
                    addToReadBlocked na ( node, fn )

                NS.ReadyToRun ->
                    addToRunnable na node
    in
    Dict.foldl classifyNode emptyAcc ns


resolveAllRunnable : Acc -> Acc
resolveAllRunnable acc =
    Dict.foldl resolveRunnable { acc | readyToRun = Dict.empty } acc.readyToRun


resolveRunnable : NodeAddr -> Node -> Acc -> Acc
resolveRunnable na n =
    let
        newNode =
            runNode n
    in
    case nodeState newNode of
        NS.ReadBlocked fn ->
            addToReadBlocked na ( newNode, fn )

        _ ->
            addToCompleted na newNode


runNode : Node -> Node
runNode node =
    case node of
        InputNode inputNode ->
            InputNode (InputNode.run inputNode)

        OutputNode outputNode ->
            OutputNode (OutputNode.run outputNode)


resolveAllReadBlocked : Acc -> Acc
resolveAllReadBlocked acc =
    Dict.foldl resolveReadBlocked { acc | readBlocked = Dict.empty } acc.readBlocked


resolveReadBlocked : NodeAddr -> ReadBlockedNode -> Acc -> Acc
resolveReadBlocked na ( n, resolver ) acc =
    case resolveWriteBlocked (addrUp na) acc of
        Just ( num, acc2 ) ->
            addToCompleted na (resolver num) acc2

        Nothing ->
            addToCompleted na n acc


resolveWriteBlocked : NodeAddr -> Acc -> Maybe ( Num, Acc )
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


resolveAllWriteBlocked : Acc -> NodeStore
resolveAllWriteBlocked acc =
    Dict.foldl (\na ( n, _, _ ) -> Dict.insert na n) acc.completed acc.writeBlocked


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        InputNode inputNode ->
            InputNode.state inputNode |> NS.map InputNode

        OutputNode outputNode ->
            OutputNode.state outputNode |> NS.map OutputNode


addToWriteBlocked : NodeAddr -> WriteBlockedNode -> Acc -> Acc
addToWriteBlocked na n acc =
    { acc | writeBlocked = Dict.insert na n acc.writeBlocked }


addToCompleted : NodeAddr -> Node -> Acc -> Acc
addToCompleted na n acc =
    { acc | completed = Dict.insert na n acc.completed }


addToReadBlocked :
    NodeAddr
    -> ReadBlockedNode
    -> { a | readBlocked : ReadBlockedStore }
    -> { a | readBlocked : ReadBlockedStore }
addToReadBlocked na n acc =
    { acc | readBlocked = Dict.insert na n acc.readBlocked }


addToRunnable : NodeAddr -> Node -> Acc -> Acc
addToRunnable na n acc =
    { acc | readyToRun = Dict.insert na n acc.readyToRun }


type alias Acc =
    { writeBlocked : WriteBlockedStore
    , readBlocked : ReadBlockedStore
    , readyToRun : NodeStore
    , completed : NodeStore
    }


emptyAcc : Acc
emptyAcc =
    Acc Dict.empty Dict.empty Dict.empty Dict.empty


type alias ReadFn a =
    () -> Maybe ( Num, a )


addrUp : NodeAddr -> NodeAddr
addrUp ( x, y ) =
    ( x, y - 1 )


view : Sim -> Html msg
view sim =
    div [ dGrid ] (Dict.toList sim.nodeStore |> List.map viewNode)


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
