module TIS100.Grid exposing (..)

import Dict exposing (Dict)
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import Utils as U


type alias Grid i o e =
    { inputs : Dict Addr ( IOConfig, i )
    , exs : Dict Addr e
    , faults : List Addr
    , outputs : Dict Addr ( IOConfig, o )
    }


init : Puzzle -> (IOConfig -> i) -> (IOConfig -> o) -> e -> Grid i o e
init puzzle ifn ofn e =
    { inputs =
        List.map (\conf -> ( ( conf.x, 0 ), ( conf, ifn conf ) )) puzzle.inputs
            |> Dict.fromList
    , outputs =
        List.map (\conf -> ( ( conf.x, 4 ), ( conf, ofn conf ) )) puzzle.outputs
            |> Dict.fromList
    , faults =
        puzzle.layout
            |> Dict.filter (\_ nk -> nk == Puzzle.Faulty)
            |> Dict.keys
    , exs =
        puzzle.layout
            |> Dict.filter (\_ nk -> nk == Puzzle.Executable)
            |> Dict.map (\_ _ -> e)
    }


type alias Addr =
    ( Int, Int )


type Node i o e
    = In IOConfig i
    | Out IOConfig o
    | Exe e
    | Fault


toList : Grid i o e -> List ( Addr, Node i o e )
toList grid =
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

        outputNodes =
            grid.outputs
                |> Dict.map (\_ ( conf, o ) -> Out conf o)
    in
    (inputNodes
        |> Dict.union outputNodes
        |> Dict.union exeNodes
        |> Dict.toList
    )
        ++ faultyNodes


foldl : (Addr -> Node i o e -> a -> a) -> a -> Grid i o e -> a
foldl fn acc grid =
    acc


replace : Addr -> Node i o e -> Grid i o e -> Grid i o e
replace addr node grid =
    grid


replaceExeEntries : List ( Addr, e ) -> Grid i o e -> Grid i o e
replaceExeEntries list grid =
    grid
