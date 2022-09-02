module TIS100.Grid exposing
    ( Addr
    , Cell(..)
    , Grid
    , init
    , inputsToList
    , outputsToList
    , replaceExeEntries
    )

import Dict exposing (Dict)
import TIS100.Puzzle as Puzzle exposing (IOConfig, Puzzle)
import Utils as U


type alias Grid i o e =
    Dict Addr (Cell i o e)


type alias Addr =
    ( Int, Int )


type Cell i o e
    = In IOConfig i
    | Out IOConfig o
    | Exe e
    | Flt


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
                            Flt
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


inputsToList : (Addr -> IOConfig -> i -> a) -> Grid i o e -> List a
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


outputsToList : (Addr -> IOConfig -> o -> a) -> Grid i o e -> List a
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
