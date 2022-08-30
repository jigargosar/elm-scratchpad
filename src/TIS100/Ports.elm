module TIS100.Ports exposing (fromPuzzle)

import Dict exposing (Dict)
import Set
import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.NodeState exposing (NodeState)
import TIS100.Num exposing (Num)
import TIS100.Puzzle as Puzzle exposing (Puzzle)
import Utils exposing (Dir4(..), moveInDir4, oppositeDir4)


type alias Ports =
    Dict Key PortValue


type alias Addr =
    ( Int, Int )



--noinspection ElmUnusedSymbol


maxX : Int
maxX =
    4


maxY : Int
maxY =
    4


type alias Key =
    ( Addr, Addr )


type alias Id =
    ( Key, Addr, Dir4 )


type PortValue
    = Empty
    | Num Num
    | Queried


toId : Addr -> IOIntent -> Id
toId addr intent =
    case intent of
        Read dir ->
            toIdHelp (moveInDir4 dir addr) (oppositeDir4 dir)

        Write dir ->
            toIdHelp addr dir


toIdHelp : Addr -> Dir4 -> Id
toIdHelp addr dir4 =
    ( ( addr, moveInDir4 dir4 addr ), addr, dir4 )


puzzleIOIds : Puzzle -> List Id
puzzleIOIds puzzle =
    List.map (\{ x } -> toId ( x, 0 ) (Write Down)) puzzle.inputs
        ++ List.map (\{ x } -> toId ( x, maxY ) (Read Up)) puzzle.outputs


puzzleLayoutDict : Puzzle -> Dict Addr Puzzle.NodeType
puzzleLayoutDict puzzle =
    Debug.todo "todo"


puzzleLayoutIds : Puzzle -> List Id
puzzleLayoutIds puzzle =
    Debug.todo "todo"


fromPuzzle : Puzzle -> List ( Addr, Dir4, PortValue )
fromPuzzle puzzle =
    let
        ids =
            puzzleIOIds puzzle
                ++ puzzleLayoutIds puzzle
                |> List.foldl (\(( key, _, _ ) as id) -> Dict.insert key id) Dict.empty
                |> Dict.values
    in
    ids |> List.map (\( _, addr, dir ) -> ( addr, dir, Empty ))


fromIOIntentsAndNodeState : Puzzle -> List ( IOIntent, NodeState a ) -> Ports
fromIOIntentsAndNodeState puzzle list =
    Debug.todo "todo"
