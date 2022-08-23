module TIS100.Sim exposing
    ( Sim
    , init
    , step
    , view
    )

import Dict exposing (Dict)
import TIS100.InputNode as InputNode exposing (InputNode)
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


type NodeState
    = WriteBlocked
    | Idle
    | ReadBlocked
    | ReadyToRun


nodeState : Node -> NodeState
nodeState node =
    Debug.todo "todo"


addToWriteBlocked : Node -> Acc -> Acc
addToWriteBlocked node acc =
    Debug.todo "todo"


addToCompleted : Node -> Acc -> Acc
addToCompleted node acc =
    Debug.todo "todo"


addToReadBlocked =
    Debug.todo "todo"


handleReadBlockedOrMarkCompleted : Node -> Acc -> Acc
handleReadBlockedOrMarkCompleted node =
    case nodeState node of
        ReadBlocked ->
            addToReadBlocked node

        _ ->
            addToCompleted node


runNode : Node -> Node
runNode node =
    Debug.todo "todo"


getReadQuery : Node -> Maybe NodeAddr
getReadQuery node =
    Debug.todo "todo"


readFromAndResolveWriteBlocked : NodeAddr -> Acc -> Maybe ( Num, Acc )
readFromAndResolveWriteBlocked nodeAddr acc =
    Debug.todo "todo"


resolveReadBlocked : Num -> Node -> Acc -> Acc
resolveReadBlocked num node acc =
    Debug.todo "todo"


stepReadBlocked : Node -> Acc -> Acc
stepReadBlocked node acc =
    case getReadQuery node of
        Just na ->
            readFromAndResolveWriteBlocked na acc
                |> Maybe.map (\( num, acc2 ) -> resolveReadBlocked num node acc2)
                |> Maybe.withDefault acc

        Nothing ->
            acc


completeAllBlocked : Acc -> NodeStore
completeAllBlocked acc =
    Debug.todo "todo"


stepNodes : NodeStore -> NodeStore
stepNodes ns =
    let
        stepperPass1 : Node -> Acc -> Acc
        stepperPass1 node =
            case nodeState node of
                WriteBlocked ->
                    addToWriteBlocked node

                Idle ->
                    addToCompleted node

                ReadBlocked ->
                    addToReadBlocked node

                ReadyToRun ->
                    handleReadBlockedOrMarkCompleted (runNode node)
    in
    Debug.todo "todo"


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
            { writeBlocked = writeBlocked
            , completed = Dict.empty
            }

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
    , completed : NodeStore
    }


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
