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
        | nodeStore = stepHelp sim.nodeStore
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
                NS.WriteBlocked ->
                    addToWriteBlocked na node

                NS.Done ->
                    addToCompleted na node

                NS.ReadBlocked ->
                    addToReadBlocked na node

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
    if isReadBlocked newNode then
        addToReadBlocked na newNode

    else
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


resolveReadBlocked : NodeAddr -> Node -> Acc -> Acc
resolveReadBlocked na n acc =
    case resolveWriteBlocked (addrUp na) acc of
        Just ( num, acc2 ) ->
            addToCompleted na (writeToNode num n) acc2

        Nothing ->
            addToCompleted na n acc


writeToNode : Num -> Node -> Node
writeToNode num node =
    Debug.todo "todo"


resolveWriteBlocked : NodeAddr -> Acc -> Maybe ( Num, Acc )
resolveWriteBlocked na acc =
    Debug.todo "todo"


resolveAllWriteBlocked : Acc -> NodeStore
resolveAllWriteBlocked acc =
    Debug.todo "todo"


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        InputNode inputNode ->
            InputNode.state inputNode |> NS.map InputNode

        OutputNode outputNode ->
            OutputNode.state outputNode |> NS.map OutputNode


isReadBlocked : Node -> Bool
isReadBlocked node =
    nodeState node == NS.ReadBlocked


addToWriteBlocked : NodeAddr -> Node -> Acc -> Acc
addToWriteBlocked na n acc =
    { acc | writeBlocked = Dict.insert na n acc.writeBlocked }


addToCompleted : NodeAddr -> Node -> Acc -> Acc
addToCompleted na n acc =
    { acc | completed = Dict.insert na n acc.completed }


addToReadBlocked : NodeAddr -> Node -> Acc -> Acc
addToReadBlocked na n acc =
    { acc | readBlocked = Dict.insert na n acc.readBlocked }


addToRunnable : NodeAddr -> Node -> Acc -> Acc
addToRunnable na n acc =
    { acc | readyToRun = Dict.insert na n acc.readyToRun }


stepHelp : NodeStore -> NodeStore
stepHelp ns =
    let
        completeWriteBlocked : Acc -> NodeStore
        completeWriteBlocked { writeBlocked, completed } =
            Dict.union writeBlocked completed
    in
    let
        ( writeBlocked, pending ) =
            Dict.partition (\_ -> isWriteBlocked) ns

        initialAcc : Acc
        initialAcc =
            { emptyAcc | writeBlocked = writeBlocked }

        stepper addr node acc =
            let
                readFn =
                    initReadFn addr acc.writeBlocked
            in
            accUpdate addr (stepNode readFn node) acc
    in
    Dict.foldl stepper initialAcc pending |> completeWriteBlocked


type alias Acc =
    { writeBlocked : NodeStore
    , readBlocked : NodeStore
    , readyToRun : NodeStore
    , completed : NodeStore
    }


emptyAcc : Acc
emptyAcc =
    Acc Dict.empty Dict.empty Dict.empty Dict.empty


accUpdate : NodeAddr -> ( Node, Maybe NodeEntry ) -> Acc -> Acc
accUpdate addr ( node, mbEntry ) acc =
    acc
        |> accAdd ( addr, node )
        |> accAddMaybe mbEntry


accAdd : NodeEntry -> Acc -> Acc
accAdd ( na, n ) acc =
    { acc
        | writeBlocked = acc.writeBlocked |> Dict.remove na
        , completed = acc.completed |> Dict.insert na n
    }


accAddMaybe : Maybe NodeEntry -> Acc -> Acc
accAddMaybe mbEntry acc =
    case mbEntry of
        Just entry ->
            accAdd entry acc

        Nothing ->
            acc


type alias ReadFn a =
    () -> Maybe ( Num, a )


addrUp : NodeAddr -> NodeAddr
addrUp ( x, y ) =
    ( x, y - 1 )


initReadFn : NodeAddr -> NodeStore -> ReadFn NodeEntry
initReadFn addr writeBlocked () =
    getEntry (addrUp addr) writeBlocked
        |> Maybe.andThen
            (\( na, n ) ->
                readNode n |> Maybe.map (mapSecond (pair na))
            )


stepNode : ReadFn a -> Node -> ( Node, Maybe a )
stepNode readFn node =
    case node of
        InputNode inputNode ->
            ( InputNode (InputNode.step inputNode), Nothing )

        OutputNode outputNode ->
            OutputNode.step readFn outputNode
                |> mapFirst OutputNode


readNode : Node -> Maybe ( Num, Node )
readNode node =
    case node of
        InputNode inputNode ->
            InputNode.read inputNode
                |> Maybe.map (mapSecond InputNode)

        OutputNode _ ->
            Nothing


isWriteBlocked : Node -> Bool
isWriteBlocked =
    readNode >> maybeToBool


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
