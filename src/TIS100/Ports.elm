module TIS100.Ports exposing
    ( Action(..)
    , Intent(..)
    , ViewModel
    , view
    , viewAllPorts
    )

import Dict exposing (Dict)
import Set exposing (Set)
import TIS100.Addr as Addr exposing (Addr)
import TIS100.Num as Num exposing (Num)
import TIS100.Puzzle as Puzzle exposing (Puzzle)
import TIS100.UI as UI
import Utils exposing (..)


type Intent
    = Read Dir4
    | Write Dir4


type Action
    = Reading Dir4
    | Writing Dir4 Num


type alias Key =
    ( Addr, Addr )


type Value
    = Empty
    | Num Num
    | Query


type alias Ports =
    Dict Key Port


type alias Port =
    ( Addr, Dir4, Value )


type alias PortEntry =
    ( Key, Port )


fromPuzzle : Puzzle -> Ports
fromPuzzle puzzle =
    addWriteIntents (Puzzle.validWrites puzzle) Dict.empty


fromViewModel : Puzzle -> ViewModel -> Ports
fromViewModel puzzle { intents, actions } =
    let
        ports : Ports
        ports =
            Dict.empty
                |> addIntents intents
                |> addActions actions
    in
    keepPorts (Puzzle.validWrites puzzle) ports


keepPorts : List ( Addr, Dir4 ) -> Ports -> Ports
keepPorts writeIntents ports =
    let
        validKeys : Set.Set Key
        validKeys =
            writeIntents
                |> List.map (\( a, d ) -> toKey a d)
                |> Set.fromList
    in
    dictKeepKeys validKeys ports


addWriteIntents : List ( Addr, Dir4 ) -> Ports -> Ports
addWriteIntents =
    let
        entry ( addr, dir4 ) =
            toEntry addr dir4 Empty
    in
    addPortEntriesBy entry


addIntents : List ( Addr, Intent ) -> Ports -> Ports
addIntents =
    let
        entry ( addr, intent ) =
            case intent of
                Read dir4 ->
                    toEntry (moveInDir4 dir4 addr) (oppositeDir4 dir4) Empty

                Write dir4 ->
                    toEntry addr dir4 Empty
    in
    addPortEntriesBy entry


addActions : List ( Addr, Action ) -> Ports -> Ports
addActions =
    let
        entry ( addr, action ) =
            case action of
                Reading dir4 ->
                    toEntry (moveInDir4 dir4 addr) (oppositeDir4 dir4) Query

                Writing dir4 num ->
                    toEntry addr dir4 (Num num)
    in
    addPortEntriesBy entry


addPortEntriesBy : (a -> PortEntry) -> List a -> Ports -> Ports
addPortEntriesBy efn ls ports =
    let
        updatePortEntry : PortEntry -> Ports -> Ports
        updatePortEntry ( key, prt ) =
            Dict.update key
                (Maybe.map (updateValue (toValue prt))
                    >> Maybe.withDefault prt
                    >> Just
                )
    in
    List.foldl (efn >> updatePortEntry) ports ls


toEntry : Addr -> Dir4 -> Value -> PortEntry
toEntry addr dir val =
    let
        key =
            toKey addr dir
    in
    ( key, ( addr, dir, val ) )


toKey : Addr -> Dir4 -> Key
toKey addr dir =
    ( addr, moveInDir4 dir addr )


toValue : Port -> Value
toValue ( _, _, v ) =
    v


mapValue : (Value -> Value) -> Port -> Port
mapValue fn ( a, b, c ) =
    ( a, b, fn c )


updateValue : Value -> Port -> Port
updateValue new =
    mapValue
        (\old ->
            case ( old, new ) of
                ( Empty, _ ) ->
                    new

                ( _, Empty ) ->
                    old

                ( _, Query ) ->
                    old

                _ ->
                    new
        )


viewAllPorts : Puzzle -> List (Html msg)
viewAllPorts puzzle =
    viewPorts (fromPuzzle puzzle)


type alias ViewModel =
    { intents : List ( Addr, Intent )
    , actions : List ( Addr, Action )
    }


view : Puzzle -> ViewModel -> List (Html msg)
view puzzle vm =
    viewPorts (fromViewModel puzzle vm)


viewPorts : Ports -> List (Html msg)
viewPorts ports =
    ports |> Dict.values |> List.map viewPort


viewPort : ( Addr, Dir4, Value ) -> Html msg
viewPort ( ( _, y ) as addr, dir, val ) =
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
viewPortHelp addr attrs =
    div
        (Addr.toGridArea addr
            :: displayFlex
            :: itemsCenter
            :: positionRelative
            :: attrs
        )


viewValue : Value -> Html msg
viewValue val =
    div [ displayGrid, placeContentCenter ]
        [ case val of
            Empty ->
                text ""

            Num num ->
                Num.view num

            Query ->
                text "?"
        ]


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
