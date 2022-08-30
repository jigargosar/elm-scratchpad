module TIS100.Ports exposing (fromPuzzle)

import Dict exposing (Dict)
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



--noinspection ElmUnusedSymbol


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


puzzleLayoutIds : Puzzle -> List Id
puzzleLayoutIds puzzle =
    let
        layoutDict : Dict Addr Puzzle.NodeType
        layoutDict =
            Puzzle.layout puzzle
                |> List.indexedMap (\i nt -> ( ( modBy 4 i, i // 4 + 1 ), nt ))
                |> Dict.fromList

        isKeyValid : Key -> Bool
        isKeyValid ( from, to ) =
            case Utils.dictGet2 from to layoutDict of
                Just ( writer, _ ) ->
                    case writer of
                        Puzzle.Executable ->
                            True

                        Puzzle.Faulty ->
                            False

                Nothing ->
                    False

        idsFromAddr : Addr -> List Id
        idsFromAddr addr =
            List.map (toIdHelp addr) [ Up, Down, Left, Right ]
                |> List.filter (\( key, _, _ ) -> isKeyValid key)
    in
    List.concatMap idsFromAddr (Dict.keys layoutDict)


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



--noinspection ElmUnusedSymbol


fromIOIntentsAndNodeState : Puzzle -> List ( IOIntent, NodeState a ) -> Ports
fromIOIntentsAndNodeState _ _ =
    Debug.todo "todo"
