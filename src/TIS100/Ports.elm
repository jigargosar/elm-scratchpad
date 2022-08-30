module TIS100.Ports exposing (viewAllPorts)

import Dict exposing (Dict)
import TIS100.IOIntent exposing (IOAction(..), IOIntent(..))
import TIS100.Num as Num exposing (Num)
import TIS100.Puzzle as Puzzle exposing (Puzzle)
import TIS100.UI as UI
import Utils exposing (..)


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


toKey : Addr -> Dir4 -> Key
toKey addr dir4 =
    ( addr, moveInDir4 dir4 addr )


puzzleLayoutIOIntents : Puzzle -> List ( Addr, IOIntent )
puzzleLayoutIOIntents puzzle =
    let
        layoutDict : Dict Addr Puzzle.NodeType
        layoutDict =
            puzzle.layout
                |> List.indexedMap (\i nt -> ( ( modBy 4 i, i // 4 + 1 ), nt ))
                |> Dict.fromList

        ioIntentsFromAddr : Addr -> List ( Addr, IOIntent )
        ioIntentsFromAddr addr =
            [ Up, Down, Left, Right ]
                |> List.filterMap
                    (\dir ->
                        let
                            to =
                                moveInDir4 dir addr
                        in
                        case dictGet2 addr to layoutDict of
                            Just ( writer, _ ) ->
                                case writer of
                                    Puzzle.Executable ->
                                        Just ( addr, Write dir )

                                    Puzzle.Faulty ->
                                        Nothing

                            Nothing ->
                                Nothing
                    )
    in
    List.concatMap ioIntentsFromAddr (Dict.keys layoutDict)


type alias Ports =
    Dict Key Port


type alias Port =
    ( Addr, Dir4, PortValue )


fromIntents : List ( Addr, IOIntent ) -> Ports
fromIntents =
    List.foldl addIntent Dict.empty


addIntent : ( Addr, IOIntent ) -> Ports -> Ports
addIntent ( addr, iOIntent ) =
    let
        port_ =
            case iOIntent of
                Read dir4 ->
                    ( moveInDir4 dir4 addr, oppositeDir4 dir4, Empty )

                Write dir4 ->
                    ( addr, dir4, Empty )
    in
    addPort port_


fromActions : List ( Addr, IOAction ) -> Ports
fromActions =
    List.foldl addAction Dict.empty


addAction : ( Addr, IOAction ) -> Ports -> Ports
addAction ( addr, iOAction ) =
    let
        port_ =
            case iOAction of
                Reading dir4 ->
                    ( moveInDir4 dir4 addr, oppositeDir4 dir4, Queried )

                Writing dir4 num ->
                    ( addr, dir4, Num num )
    in
    addPort port_


addPort : Port -> Ports -> Ports
addPort (( addr, dir, new ) as port_) =
    Dict.update (toKey addr dir)
        (\mbPort ->
            case mbPort of
                Nothing ->
                    Just port_

                Just ( _, _, old ) ->
                    let
                        val =
                            case ( old, new ) of
                                ( Empty, _ ) ->
                                    new

                                ( _, Empty ) ->
                                    old

                                ( _, Queried ) ->
                                    old

                                _ ->
                                    new
                    in
                    Just ( addr, dir, val )
        )


allPuzzlePorts : Puzzle -> Ports
allPuzzlePorts puzzle =
    let
        ioIntents : List ( Addr, IOIntent )
        ioIntents =
            List.map (\{ x } -> ( ( x, 0 ), Write Down )) puzzle.inputs
                ++ List.map (\{ x } -> ( ( x, maxY ), Read Up )) puzzle.outputs
                ++ puzzleLayoutIOIntents puzzle
    in
    fromIntents ioIntents


viewAllPorts : Puzzle -> List (Html msg)
viewAllPorts puzzle =
    allPuzzlePorts puzzle |> Dict.values |> List.map viewPort


view : Puzzle -> List ( Addr, IOIntent ) -> List ( Addr, IOAction ) -> List (Html msg)
view puzzle intents actions =
    let
        portsWithValue =
            fromIntents intents
                |> Dict.union (fromActions actions)

        ports =
            allPuzzlePorts puzzle
                |> Dict.intersect (fromIntents intents)
    in
    Debug.todo "todo"


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
            , viewPortValue portValue
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
            [ viewPortValue portValue
            , viewArrow Up portValue
            ]
        ]


viewPortValue : PortValue -> Html msg
viewPortValue =
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
                    UI.darkGray

                _ ->
                    "inherit"
    in
    span [ fg color, fontSize "2em", fontWeight "100" ] [ text (arrowDefault dir4) ]



-- ARROW


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
