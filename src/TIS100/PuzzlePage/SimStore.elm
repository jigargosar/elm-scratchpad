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
import TIS100.ExeNode as ExeNode exposing (ExeNode)
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
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


init : Puzzle -> List ( Addr, ExeNode ) -> Model
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


replaceExeEntries : List ( Addr, ExeNode ) -> Model -> Model
replaceExeEntries list grid =
    List.foldl replaceExe grid list


replaceExe : ( Addr, ExeNode ) -> Model -> Model
replaceExe ( addr, e ) =
    U.mapKey addr
        (\n ->
            case n of
                Exe _ ->
                    Exe e

                _ ->
                    n
        )


intentsOf : Node -> List Intent
intentsOf node =
    case node of
        In _ _ ->
            [ Write U.Down ]

        Out _ _ ->
            [ Read U.Up ]

        Exe exe ->
            ExeNode.intents exe

        Flt ->
            []


actionsOf : Node -> List Action
actionsOf node =
    case node of
        Out _ _ ->
            []

        _ ->
            case toStepState node of
                SR.ReadyToRun _ ->
                    []

                SR.ReadBlocked dir4 _ ->
                    [ Reading dir4 ]

                SR.WriteBlocked num dir4 _ ->
                    [ Writing dir4 num ]

                SR.Done ->
                    []


toStepState : Node -> SR.NodeState Node
toStepState node =
    case node of
        In conf inputNode ->
            InputNode.stepState inputNode |> SR.map (In conf)

        Out conf outputNode ->
            OutputNode.stepState outputNode |> SR.map (Out conf)

        Exe exeNode ->
            ExeNode.stepState exeNode |> SR.map Exe

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


leftBarViewModel : Model -> LB.ViewModel
leftBarViewModel simStore =
    { inputs = leftBarInputs simStore
    , outputs = leftBarOutputs simStore
    }


leftBarInputs : Model -> List LB.Input
leftBarInputs model =
    Dict.values model
        |> List.filterMap
            (\node ->
                case node of
                    In c i ->
                        Just
                            { title = c.title
                            , nums = InputNode.toSelectionList i
                            }

                    _ ->
                        Nothing
            )


leftBarOutputs : Model -> List LB.Output
leftBarOutputs model =
    Dict.values model
        |> List.filterMap
            (\node ->
                case node of
                    Out { title, nums } o ->
                        let
                            actual =
                                OutputNode.getNumsRead o
                        in
                        { title = title
                        , expected = SelectionList.fromIndex (List.length actual) nums
                        , actual = actual
                        }
                            |> Just

                    _ ->
                        Nothing
            )


step : Model -> Model
step store =
    SR.step toStepState store
