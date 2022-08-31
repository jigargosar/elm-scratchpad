module TIS100.Ports exposing (view, viewAllPorts)

import Dict exposing (Dict)
import TIS100.IOIntent exposing (IOAction(..), IOIntent(..))
import TIS100.Num as Num exposing (Num)
import TIS100.Puzzle as Puzzle exposing (Puzzle)
import TIS100.UI as UI
import Utils exposing (..)


type alias Addr =
    ( Int, Int )


maxY : Int
maxY =
    4


type alias Key =
    ( Addr, Addr )


type alias Id =
    ( Key, Addr, Dir4 )


type Value
    = Empty
    | Num Num
    | Query


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
    ( Addr, Dir4, Value )


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
                    ( moveInDir4 dir4 addr, oppositeDir4 dir4, Query )

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

                                ( _, Query ) ->
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
    allPuzzlePorts puzzle |> viewPorts


view : Puzzle -> ( List ( Addr, IOIntent ), List ( Addr, IOAction ) ) -> List (Html msg)
view puzzle ( intents, actions ) =
    let
        selectedPorts =
            fromIntents intents
                |> Dict.union (fromActions actions)

        ports =
            allPuzzlePorts puzzle
                |> Dict.intersect selectedPorts
    in
    viewPorts ports


viewPorts : Ports -> List (Html msg)
viewPorts ports =
    ports |> Dict.values |> List.map viewPort


viewPort : ( Addr, Dir4, Value ) -> Html msg
viewPort ( ( x, y ) as addr, dir, val ) =
    case dir of
        Up ->
            viewPortHelp addr
                [ flexRow
                , style "justify-content" "end"
                , pr "0.5ch"
                , sHeight UI.gapSize
                , sWidth "50%"
                , style "bottom" UI.gapSize
                ]
                [ viewValue val
                , viewArrow Up val
                ]

        Down ->
            viewPortHelp addr
                [ flexRow
                , style "justify-content" "start"
                , pl "0.5ch"
                , sHeight UI.gapSize
                , sWidth "50%"
                , if y == 0 then
                    style "bottom" UI.gapSize

                  else
                    top100
                , style "left" "50%"
                ]
                [ viewArrow Down val
                , viewValue val
                ]

        Right ->
            viewPortHelp addr
                [ flexColumn
                , style "justify-content" "end"
                , pb "0.5ch"
                , sHeight "50%"
                , sWidth UI.gapSize
                , left100
                ]
                [ viewValue val
                , viewArrow Right val
                ]

        Left ->
            viewPortHelp addr
                [ flexColumn
                , style "justify-content" "start"
                , pt "0.5ch"
                , sHeight "50%"
                , sWidth UI.gapSize
                , style "top" "50%"
                , style "right" UI.gapSize
                ]
                [ viewArrow Left val
                , viewValue val
                ]


viewPortHelp : Addr -> List (Attribute msg) -> List (Html msg) -> Html msg
viewPortHelp ( x, y ) attrs =
    div
        (gridAreaXY ( x, y - 1 |> atLeast 0 )
            :: displayFlex
            :: itemsCenter
            :: positionRelative
            :: attrs
        )


viewValue : Value -> Html msg
viewValue val =
    let
        valStr =
            case val of
                Empty ->
                    ""

                Num num ->
                    Num.toString num

                Query ->
                    "?"
    in
    div [ displayGrid, placeContentCenter ] [ text valStr ]


viewArrow : Dir4 -> Value -> Html msg
viewArrow dir4 pv =
    let
        color =
            case pv of
                Empty ->
                    UI.darkGray

                _ ->
                    "inherit"
    in
    div
        [ displayGrid
        , placeContentCenter
        , fg color
        , fontSize "2em"
        , fontWeight "100"
        ]
        [ text (arrowDefault dir4) ]



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
