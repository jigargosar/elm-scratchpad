module TIS100.Ports exposing (fromPuzzle)

import Dict exposing (Dict)
import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.NodeState exposing (NodeState)
import TIS100.Num exposing (Num)
import TIS100.Puzzle exposing (Puzzle)
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


type PortValue
    = Empty
    | Num Num
    | Queried


empty : Ports
empty =
    Debug.todo "todo"


fromPuzzleIO : Puzzle -> Ports
fromPuzzleIO puzzle =
    Debug.todo "todo"


toKey : Addr -> IOIntent -> Key
toKey addr intent =
    case intent of
        Read dir ->
            toKeyHelp (moveInDir4 dir addr) (oppositeDir4 dir)

        Write dir ->
            toKeyHelp addr dir


toKeyHelp : Addr -> Dir4 -> Key
toKeyHelp addr dir4 =
    ( addr, moveInDir4 dir4 addr )


fromPuzzle : Puzzle -> List ( Addr, Dir4, PortValue )
fromPuzzle puzzle =
    let
        ioKeys : List Key
        ioKeys =
            List.map (\{ x } -> toKey ( x, 0 ) (Write Down)) puzzle.inputs
                ++ List.map (\{ x } -> toKey ( x, maxY ) (Read Up)) puzzle.outputs
    in
    Debug.todo "todo"


fromIOIntentsAndNodeState : Puzzle -> List ( IOIntent, NodeState a ) -> Ports
fromIOIntentsAndNodeState puzzle list =
    Debug.todo "todo"
