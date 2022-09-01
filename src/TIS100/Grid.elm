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


type alias Addr =
    ( Int, Int )


type Node i o e
    = In IOConfig i
    | Out IOConfig o
    | Exe e
    | Fault


init : Puzzle -> (IOConfig -> i) -> (IOConfig -> o) -> e -> Grid i o e
init puzzle ifn ofn e =
    let
        io =
            List.map (\c -> ( ( c.x, 0 ), In c (ifn c) )) puzzle.inputs
                ++ List.map (\c -> ( ( c.x, 4 ), Out c (ofn c) )) puzzle.outputs
                |> Dict.fromList

        layout =
            Dict.map
                (\_ nk ->
                    case nk of
                        Puzzle.Executable ->
                            Exe e

                        Puzzle.Faulty ->
                            Fault
                )
                puzzle.layout
    in
    Dict.union io layout


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
