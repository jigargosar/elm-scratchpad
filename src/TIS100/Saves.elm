module TIS100.Saves exposing (Saves, fromList, get, set)

import Dict exposing (Dict)
import TIS100.Addr exposing (Addr)
import TIS100.Puzzle as Puzzle
import Utils exposing (mapFirst)


type Saves
    = Saves (Dict String (List ( Addr, String )))


fromList : List ( Puzzle.Id, List ( Addr, String ) ) -> Saves
fromList ls =
    Saves (ls |> List.map (mapFirst idToString) |> Dict.fromList)


idToString : Puzzle.Id -> String
idToString name =
    case name of
        Puzzle.SignalComparator ->
            "SignalComparator"


set : Puzzle.Id -> List ( Addr, String ) -> Saves -> Saves
set name srcEntries (Saves dict) =
    Saves (Dict.insert (idToString name) srcEntries dict)


get : Puzzle.Id -> Saves -> List ( Addr, String )
get name (Saves dict) =
    Dict.get (idToString name) dict
        |> Maybe.withDefault []
