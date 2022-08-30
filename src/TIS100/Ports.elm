module TIS100.Ports exposing (fromPuzzle)

import Dict exposing (Dict)
import Html exposing (Html)
import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.NodeState exposing (NodeState)
import TIS100.Num as Num exposing (Num)
import TIS100.Puzzle as Puzzle exposing (Puzzle)
import Utils exposing (..)


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


viewPort : ( Addr, Dir4, PortValue ) -> Html msg
viewPort ( addr, dir, portValue ) =
    case dir of
        Up ->
            viewUpPortValue addr portValue

        Down ->
            viewDownPortValue addr portValue

        Left ->
            noView

        Right ->
            noView


viewDownPortValue : Addr -> PortValue -> Html msg
viewDownPortValue ( x, y ) portValue =
    gtCols 2
        [ gridAreaXY ( x * 2, y * 2 ), noPointerEvents ]
        [ fRow
            [ gridAreaXY ( 1, 0 )
            , allPointerEvents
            , itemsCenter
            , pl "1ch"
            , gap "1ch"
            ]
            [ viewArrow Down portValue
            , viewPortValueText portValue
            ]
        ]


viewUpPortValue : Addr -> PortValue -> Html msg
viewUpPortValue ( x, y ) portValue =
    gtCols 2
        [ gridAreaXY ( x * 2, (y * 2) - 2 ), noPointerEvents ]
        [ fRow
            [ gridAreaXY ( 0, 0 )
            , allPointerEvents
            , itemsCenter
            , justifyContent "end"
            , pr "1ch"
            , gap "1ch"
            ]
            [ viewPortValueText portValue
            , viewArrow Up portValue
            ]
        ]


viewPortValueText : PortValue -> Html msg
viewPortValueText =
    let
        toString portValue =
            case portValue of
                Empty ->
                    ""

                Num num ->
                    Num.toString num

                Queried ->
                    "?"
    in
    toString >> text


viewArrow : Dir4 -> PortValue -> Html msg
viewArrow dir4 pv =
    let
        color =
            case pv of
                Empty ->
                    darkGray

                _ ->
                    "inherit"
    in
    span [ fg color, fontSize "2em", fontWeight "100" ] [ text (arrowDefault dir4) ]



--noinspection ElmUnusedSymbol


fromIOIntentsAndNodeState : Puzzle -> List ( IOIntent, NodeState a ) -> Ports
fromIOIntentsAndNodeState _ _ =
    Debug.todo "todo"



-- UI HELPERS


darkGray =
    grayN 0.5


lightGray =
    grayN 0.7


lightOutline =
    sOutline ("1px solid " ++ lightGray)


type ArrowType
    = Filled
    | Outline


defaultArrowType =
    Filled


arrowDefault =
    case defaultArrowType of
        Filled ->
            arrowFilled

        Outline ->
            arrowOutline


arrowOutline : Dir4 -> String
arrowOutline dir4 =
    case dir4 of
        Up ->
            "⇧"

        Down ->
            "⇩"

        Left ->
            "⇦ "

        Right ->
            "⇨"


arrowFilled : Dir4 -> String
arrowFilled dir4 =
    case dir4 of
        Up ->
            "⬆"

        Down ->
            "⬇"

        Left ->
            "⬅"

        Right ->
            "➡"
