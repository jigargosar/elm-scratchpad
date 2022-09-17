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
import TIS100.ExeNode as EN exposing (ExeNode)
import TIS100.InputNode as IN exposing (InputNode)
import TIS100.OutputNode as ON exposing (OutputNode)
import TIS100.Ports as Ports exposing (Action(..), Intent(..))
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.PuzzlePage.StepRunner as SR
import TIS100.SelectionList as SelectionList
import Utils as U exposing (Dir4)


type alias Model =
    Dict Addr Node


type Node
    = In IOConfig InputNode
    | Out IOConfig OutputNode
    | Exe ExeNode
    | Flt


type alias ExeDict =
    Dict Addr ExeNode


init : Puzzle -> ExeDict -> Model
init puzzle exs =
    let
        initIn c =
            In c (IN.fromList c.nums)

        initOut c =
            Out c (ON.fromExpected (List.length c.nums))

        initLayout addr nk =
            case nk of
                Puzzle.Executable ->
                    initExe addr

                Puzzle.Faulty ->
                    Flt

        initExe addr =
            Exe (U.dictGetOr EN.empty addr exs)
    in
    Puzzle.toDictBy
        initIn
        initOut
        initLayout
        puzzle


step : Model -> Model
step store =
    SR.step toStepRunnerState store


toStepRunnerState : Node -> SR.NodeState Node
toStepRunnerState node =
    case node of
        In conf inputNode ->
            IN.stepState inputNode |> SR.map (In conf)

        Out conf outputNode ->
            ON.stepState outputNode |> SR.map (Out conf)

        Exe exeNode ->
            EN.toStepRunnerNodeState exeNode |> SR.map Exe

        Flt ->
            SR.Done


portsViewModel : Model -> Ports.ViewModel
portsViewModel simStore =
    Dict.foldl
        (\addr node { intents, actions } ->
            { intents = List.map (U.pair addr) (intentsOf node) ++ intents
            , actions = List.map (U.pair addr) (actionsOf node) ++ actions
            }
        )
        { intents = [], actions = [] }
        simStore


intentsOf : Node -> List Intent
intentsOf node =
    case node of
        In _ _ ->
            [ Write U.Down ]

        Out _ _ ->
            [ Read U.Up ]

        Exe exe ->
            EN.intents exe

        Flt ->
            []


actionsOf : Node -> List Action
actionsOf node =
    case node of
        Out _ _ ->
            []

        _ ->
            case toStepRunnerState node of
                SR.ReadyToRun _ ->
                    []

                SR.ReadBlocked dir4 _ ->
                    [ Reading dir4 ]

                SR.WriteBlocked num dir4 _ ->
                    [ Writing dir4 num ]

                SR.Done ->
                    []


leftBarViewModel : Model -> LB.ViewModel
leftBarViewModel simStore =
    Dict.values simStore
        |> List.foldr
            (\n vm ->
                case n of
                    In c i ->
                        { vm | inputs = toLBInput c i :: vm.inputs }

                    Out c o ->
                        { vm | outputs = toLBOutput c o :: vm.outputs }

                    _ ->
                        vm
            )
            { inputs = []
            , outputs = []
            }


toLBOutput : IOConfig -> OutputNode -> LB.Output
toLBOutput c o =
    let
        actual =
            ON.getNumsRead o
    in
    { title = c.title
    , expected = SelectionList.fromIndex (List.length actual) c.nums
    , actual = actual
    }


toLBInput : IOConfig -> InputNode -> LB.Input
toLBInput c i =
    { title = c.title
    , nums = IN.toSelectionList i
    }
