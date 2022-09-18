module TIS100.PuzzlePage.SimStore exposing
    ( Model
    , Node(..)
    , init
    , leftBarViewModel
    , portsViewModel
    , step
    )

import Dict exposing (Dict)
import TIS100.Addr exposing (Addr)
import TIS100.Exe as Exe exposing (ExeNode)
import TIS100.InNode as In exposing (InNode)
import TIS100.OutNode as Out exposing (OutNode)
import TIS100.Ports as Ports exposing (Action(..), Intent(..))
import TIS100.Puzzle as Puzzle exposing (InConfig, OutConfig, Puzzle)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.PuzzlePage.NodeState as NodeState exposing (NodeState(..))
import TIS100.PuzzlePage.SimRunner as StepRunner
import TIS100.SelectionList as SelectionList
import Utils as U exposing (Dir4)


type alias Model =
    Dict Addr Node


type Node
    = IN InConfig InNode
    | OUT OutConfig OutNode
    | EXE ExeNode
    | FLT


type alias ExeDict =
    Dict Addr ExeNode


init : Puzzle -> ExeDict -> Model
init puzzle exs =
    let
        initIn : InConfig -> Node
        initIn c =
            IN c (In.fromList (Puzzle.inNums c))

        initOut c =
            OUT c (Out.fromExpected (List.length c.nums))

        initExe addr =
            EXE (U.dictGetOr Exe.empty addr exs)
    in
    Puzzle.toDictBy
        { in_ = initIn
        , out = initOut
        , exe = initExe
        , flt = always FLT
        }
        puzzle


step : Model -> Model
step store =
    StepRunner.step nodeState store


nodeState : Node -> NodeState Node
nodeState node =
    case node of
        IN conf i ->
            In.stepState i |> NodeState.map (IN conf)

        OUT conf out ->
            Out.stepState out |> NodeState.map (OUT conf)

        EXE exe ->
            Exe.toState exe |> NodeState.map EXE

        FLT ->
            NodeState.Idle


portsViewModel : Model -> Ports.ViewModel
portsViewModel simStore =
    Dict.foldl
        (\addr node { intents, actions } ->
            { intents = List.map (U.pair addr) (nodeIntents node) ++ intents
            , actions = List.map (U.pair addr) (nodeActions node) ++ actions
            }
        )
        { intents = [], actions = [] }
        simStore


nodeIntents : Node -> List Intent
nodeIntents node =
    case node of
        IN _ _ ->
            [ Write U.Down ]

        OUT _ _ ->
            [ Read U.Up ]

        EXE exe ->
            Exe.intents exe

        FLT ->
            []


nodeActions : Node -> List Action
nodeActions node =
    case node of
        OUT _ _ ->
            []

        _ ->
            case nodeState node of
                ReadyToRun _ ->
                    []

                ReadBlocked dir4 _ ->
                    [ Reading dir4 ]

                WriteBlocked num dir4 _ ->
                    [ Writing dir4 num ]

                Idle ->
                    []


leftBarViewModel : Model -> LB.ViewModel
leftBarViewModel simStore =
    Dict.values simStore
        |> List.foldr
            (\n vm ->
                case n of
                    IN c i ->
                        { vm | inputs = toLBInput c i :: vm.inputs }

                    OUT c o ->
                        { vm | outputs = toLBOutput c o :: vm.outputs }

                    _ ->
                        vm
            )
            { inputs = []
            , outputs = []
            }


toLBInput : InConfig -> InNode -> LB.Input
toLBInput c i =
    { title = Puzzle.inTitle c
    , nums = In.toSelectionList i
    }


toLBOutput : OutConfig -> OutNode -> LB.Output
toLBOutput c o =
    let
        actual =
            Out.getNumsRead o
    in
    { title = c.title
    , expected = SelectionList.fromIndex (List.length actual) c.nums
    , actual = actual
    }
