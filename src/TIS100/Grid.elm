module TIS100.Grid exposing
    ( Grid
    , Node(..)
    , init
    , replaceExeEntries
    )

import Dict exposing (Dict)
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import Utils as U


type alias Grid i o e =
    Dict Addr (Node i o e)


replaceExeEntries : List ( Addr, e ) -> Grid i o e -> Grid i o e
replaceExeEntries list grid =
    List.foldl replaceExe grid list


replaceExe : ( Addr, e ) -> Grid i o e -> Grid i o e
replaceExe ( addr, e ) =
    U.mapKey addr
        (\n ->
            case n of
                Exe _ ->
                    Exe e

                _ ->
                    n
        )


type alias GridRec i o e =
    { inputs : Dict Addr ( IOConfig, i )
    , exs : Dict Addr e
    , faults : List Addr
    , outputs : Dict Addr ( IOConfig, o )
    }


init : Puzzle -> (IOConfig -> i) -> (IOConfig -> o) -> e -> Grid i o e
init puzzle ifn ofn e =
    let
        inputs =
            List.map (\conf -> ( ( conf.x, 0 ), In conf (ifn conf) )) puzzle.inputs
                |> Dict.fromList

        outputs =
            List.map (\conf -> ( ( conf.x, 4 ), Out conf (ofn conf) )) puzzle.outputs
                |> Dict.fromList

        faults =
            puzzle.layout
                |> Dict.filter (\_ nk -> nk == Puzzle.Faulty)
                |> Dict.map (\_ _ -> Fault)

        exs =
            puzzle.layout
                |> Dict.filter (\_ nk -> nk == Puzzle.Executable)
                |> Dict.map (\_ _ -> Exe e)
    in
    inputs
        |> Dict.union outputs
        |> Dict.union faults
        |> Dict.union exs


type alias Addr =
    ( Int, Int )


type Node i o e
    = In IOConfig i
    | Out IOConfig o
    | Exe e
    | Fault


toDict : GridRec i o e -> Grid i o e
toDict grid =
    let
        inputNodes =
            grid.inputs
                |> Dict.map (\_ ( conf, i ) -> In conf i)

        exeNodes =
            grid.exs
                |> Dict.map (\_ e -> Exe e)

        faultyNodes =
            grid.faults
                |> List.map (U.pairTo Fault)
                |> Dict.fromList

        outputNodes =
            grid.outputs
                |> Dict.map (\_ ( conf, o ) -> Out conf o)
    in
    inputNodes
        |> Dict.union outputNodes
        |> Dict.union exeNodes
        |> Dict.union faultyNodes
