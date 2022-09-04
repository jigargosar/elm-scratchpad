module TIS100.PuzzlePage.SimStore exposing
    ( SimNode(..)
    , SimStore
    , init
    , ioIntentsAndActions
    , leftBarInputs
    , leftBarOutputs
    , step
    )

import Dict exposing (Dict)
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.NodeState as S exposing (NodeState)
import TIS100.Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
import TIS100.Ports as Ports exposing (Action(..), Intent(..))
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.SelectionList as SelectionList
import Utils as U exposing (Dir4)


type alias SimStore =
    Dict Addr SimNode


type alias Addr =
    ( Int, Int )


type SimNode
    = In IOConfig InputNode
    | Out IOConfig OutputNode
    | Exe ExeNode
    | Flt


init : Puzzle -> List ( Addr, ExeNode ) -> SimStore
init puzzle exs =
    let
        io =
            List.map (\c -> ( ( c.x, 0 ), In c (InputNode.fromList c.nums) )) puzzle.inputs
                ++ List.map
                    (\c ->
                        ( ( c.x, 4 )
                        , Out c
                            (OutputNode.fromExpected (List.length c.nums))
                        )
                    )
                    puzzle.outputs
                |> Dict.fromList

        layout =
            Dict.map
                (\_ nk ->
                    case nk of
                        Puzzle.Executable ->
                            Exe ExeNode.empty

                        Puzzle.Faulty ->
                            Flt
                )
                puzzle.layout
                |> replaceExeEntries exs
    in
    Dict.union io layout


replaceExeEntries : List ( Addr, ExeNode ) -> SimStore -> SimStore
replaceExeEntries list grid =
    List.foldl replaceExe grid list


replaceExe : ( Addr, ExeNode ) -> SimStore -> SimStore
replaceExe ( addr, e ) =
    U.mapKey addr
        (\n ->
            case n of
                Exe _ ->
                    Exe e

                _ ->
                    n
        )


simNodeIntents : SimNode -> List Intent
simNodeIntents node =
    case node of
        In _ _ ->
            [ Write U.Down ]

        Out _ _ ->
            [ Read U.Up ]

        Exe exe ->
            ExeNode.intents exe

        Flt ->
            []


simNodeActions : SimNode -> List Action
simNodeActions node =
    case node of
        Out _ _ ->
            []

        _ ->
            case simNodeState node of
                S.ReadyToRun _ ->
                    []

                S.ReadBlocked dir4 _ ->
                    [ Reading dir4 ]

                S.WriteBlocked num dir4 _ ->
                    [ Writing dir4 num ]

                S.Done ->
                    []


simNodeState : SimNode -> NodeState SimNode
simNodeState node =
    case node of
        In conf inputNode ->
            InputNode.state inputNode |> S.map (In conf)

        Out conf outputNode ->
            OutputNode.state outputNode |> S.map (Out conf)

        Exe exeNode ->
            ExeNode.state exeNode |> S.map Exe

        Flt ->
            S.Done


inputsToList : (Addr -> IOConfig -> InputNode -> a) -> SimStore -> List a
inputsToList fn grid =
    Dict.toList grid
        |> List.filterMap
            (\( addr, cell ) ->
                case cell of
                    In conf i ->
                        Just <| fn addr conf i

                    _ ->
                        Nothing
            )


outputsToList : (Addr -> IOConfig -> OutputNode -> a) -> SimStore -> List a
outputsToList fn grid =
    Dict.toList grid
        |> List.filterMap
            (\( addr, cell ) ->
                case cell of
                    Out conf o ->
                        Just <| fn addr conf o

                    _ ->
                        Nothing
            )


ioIntentsAndActions : SimStore -> Ports.IOIntentsAndActions
ioIntentsAndActions simStore =
    Dict.foldl
        (\addr node { intents, actions } ->
            { intents = List.map (U.pair addr) (simNodeIntents node) ++ intents
            , actions = List.map (U.pair addr) (simNodeActions node) ++ actions
            }
        )
        { intents = [], actions = [] }
        simStore


leftBarInputs : SimStore -> List LB.Input
leftBarInputs simStore =
    inputsToList
        (\_ c i ->
            { title = c.title
            , nums = InputNode.toSelectionList i
            }
        )
        simStore


leftBarOutputs : SimStore -> List LB.Output
leftBarOutputs simStore =
    outputsToList
        (\_ c o ->
            let
                actual =
                    OutputNode.getNumsRead o
            in
            { title = c.title
            , expected =
                SelectionList.fromIndex (List.length actual) c.nums
            , actual = actual
            }
        )
        simStore


step : SimStore -> SimStore
step store =
    Dict.foldl stepNode (initAcc store) store
        |> resolveAllReadBlocked
        |> resolveAllWriteBlocked


stepNode : Addr -> SimNode -> Acc -> Acc
stepNode addr node =
    case simNodeState node of
        S.WriteBlocked num dir cont ->
            addToWriteBlocked addr node num dir cont

        S.Done ->
            addToCompleted addr node

        S.ReadBlocked dir cont ->
            addToReadBlocked addr node dir cont

        S.ReadyToRun cont ->
            resolveAfterRun addr (cont ())


resolveAfterRun : Addr -> SimNode -> Acc -> Acc
resolveAfterRun addr node =
    case simNodeState node of
        S.ReadBlocked dir cont ->
            addToReadBlocked addr node dir cont

        _ ->
            addToCompleted addr node


resolveAllReadBlocked : Acc -> Acc
resolveAllReadBlocked acc =
    Dict.foldl resolveReadBlocked { acc | readBlocked = Dict.empty } acc.readBlocked


resolveReadBlocked :
    Addr
    -> ReadBlockedNode
    -> WriteBlockedAcc a
    -> WriteBlockedAcc a
resolveReadBlocked addr ( node, dir, cont ) acc =
    case readAndUnblock addr dir acc of
        Just ( num, acc2 ) ->
            addToCompleted addr (cont num) acc2

        Nothing ->
            addToCompleted addr node acc


readAndUnblock :
    Addr
    -> Dir4
    -> WriteBlockedAcc a
    -> Maybe ( Num, WriteBlockedAcc a )
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


resolveAllWriteBlocked : WriteBlockedAcc a -> SimStore
resolveAllWriteBlocked acc =
    Dict.foldl (\addr { node } -> U.replaceEntry ( addr, node )) acc.completed acc.writeBlocked


type alias Acc =
    { readBlocked : ReadBlockedStore
    , writeBlocked : WriteBlockedStore
    , completed : SimStore
    }


type alias WriteBlockedAcc a =
    { a
        | writeBlocked : WriteBlockedStore
        , completed : SimStore
    }


type alias ReadBlockedStore =
    Dict Addr ReadBlockedNode


type alias ReadBlockedNode =
    ( SimNode, Dir4, Num -> SimNode )


type alias WriteBlockedStore =
    Dict Addr WriteBlockedNode


type alias WriteBlockedNode =
    { node : SimNode, num : Num, dir : Dir4, cont : () -> SimNode }


initAcc : SimStore -> Acc
initAcc store =
    { readBlocked = Dict.empty
    , writeBlocked = Dict.empty
    , completed = store
    }


addToReadBlocked :
    Addr
    -> SimNode
    -> Dir4
    -> (Num -> SimNode)
    -> { a | readBlocked : ReadBlockedStore }
    -> { a | readBlocked : ReadBlockedStore }
addToReadBlocked addr node dir cont acc =
    { acc | readBlocked = Dict.insert addr ( node, dir, cont ) acc.readBlocked }


addToWriteBlocked :
    Addr
    -> SimNode
    -> Num
    -> Dir4
    -> (() -> SimNode)
    -> { a | writeBlocked : WriteBlockedStore }
    -> { a | writeBlocked : WriteBlockedStore }
addToWriteBlocked addr node num dir cont acc =
    let
        wbn =
            WriteBlockedNode node num dir cont
    in
    { acc | writeBlocked = Dict.insert addr wbn acc.writeBlocked }


addToCompleted :
    Addr
    -> SimNode
    -> { a | completed : SimStore }
    -> { a | completed : SimStore }
addToCompleted na n acc =
    { acc | completed = U.replaceEntry ( na, n ) acc.completed }


completeWriteBlocked : Addr -> SimNode -> WriteBlockedAcc a -> WriteBlockedAcc a
completeWriteBlocked addr node acc =
    { acc | writeBlocked = Dict.remove addr acc.writeBlocked }
        |> addToCompleted addr node
