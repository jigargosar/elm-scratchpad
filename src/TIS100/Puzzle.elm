module TIS100.Puzzle exposing
    ( IOConfig
    , NodeType(..)
    , Puzzle
    , executableAddresses
    , samplePuzzle
    , toDictBy
    , toListBy
    , validWrites
    )

import Dict exposing (Dict)
import TIS100.Addr exposing (Addr)
import TIS100.Num as Num exposing (Num)
import Utils as U exposing (Dir4(..), pair)


type alias Puzzle =
    { title : String
    , description : List String
    , inputs : List IOConfig
    , outputs : List IOConfig
    , layout : Layout
    }


type alias Layout =
    Dict Addr NodeType


type alias IOConfig =
    { x : Int
    , title : String
    , nums : List Num
    }


type NodeType
    = Executable
    | Faulty


samplePuzzle : Puzzle
samplePuzzle =
    { title = "Differential Converter"
    , description =
        [ "READ VALUES FROM IN.A AND IN.B"
        , "WRITE IN.A - IN.B TO OUT.P"
        , "WRITE IN.B - IN.A TO OUT.N"
        ]
    , inputs =
        [ { x = 0, title = "IN.A", nums = Num.range 1 20 }
        , { x = 1, title = "IN.B", nums = Num.range 1 20 }
        ]
    , outputs =
        [ { x = 0, title = "OUT.P", nums = Num.range 1 20 }
        , { x = 1, title = "OUT.N", nums = Num.range 1 20 }
        ]
    , layout =
        [ [ Executable, Executable, Executable, Executable ]
        , [ Executable, Executable, Faulty, Executable ]
        , [ Executable, Executable, Executable, Executable ]
        ]
            |> toLayout
    }


toLayout : List (List NodeType) -> Layout
toLayout lss =
    List.concat lss
        ++ List.repeat 12 Executable
        |> List.take 12
        |> List.indexedMap pair
        |> List.filterMap
            (\( i, nk ) ->
                Just ( ( modBy 4 i, i // 4 + 1 ), nk )
            )
        |> Dict.fromList


toListBy :
    (IOConfig -> v)
    -> (IOConfig -> v)
    -> (( Addr, NodeType ) -> v)
    -> Puzzle
    -> List v
toListBy ifn ofn lfn puzzle =
    let
        io =
            List.map (\c -> ifn c) puzzle.inputs
                ++ List.map (\c -> ofn c) puzzle.outputs

        layout : List v
        layout =
            List.map lfn (Dict.toList puzzle.layout)
    in
    io ++ layout


validWrites : Puzzle -> List ( Addr, Dir4 )
validWrites puzzle =
    let
        isValidWriteDestination : Addr -> Bool
        isValidWriteDestination addr =
            Dict.member addr puzzle.layout

        isValidLayoutWrite : ( Addr, Dir4 ) -> Bool
        isValidLayoutWrite ( addr, dir4 ) =
            isValidWriteDestination (U.moveInDir4 dir4 addr)

        allWrites : Addr -> List ( Addr, Dir4 )
        allWrites addr =
            List.map (pair addr) [ Up, Down, Left, Right ]

        executableNodeWrites : Addr -> List ( Addr, Dir4 )
        executableNodeWrites addr =
            allWrites addr |> List.filter isValidLayoutWrite

        nodeWrites : ( Addr, NodeType ) -> List ( Addr, Dir4 )
        nodeWrites ( addr, nt ) =
            case nt of
                Executable ->
                    executableNodeWrites addr

                Faulty ->
                    []
    in
    toListBy
        (\c -> [ ( inputAddr c, Down ) ])
        (\c -> [ ( addrAboveOutput c, Down ) ])
        nodeWrites
        puzzle
        |> List.concat


toDictBy :
    (IOConfig -> v)
    -> (IOConfig -> v)
    -> (Addr -> NodeType -> v)
    -> Puzzle
    -> Dict Addr v
toDictBy ifn ofn lfn puzzle =
    let
        io =
            List.map (\c -> ( inputAddr c, ifn c )) puzzle.inputs
                ++ List.map (\c -> ( outputAddr c, ofn c )) puzzle.outputs
                |> Dict.fromList

        layout =
            Dict.map lfn puzzle.layout
    in
    Dict.union io layout


executableAddresses : Puzzle -> List Addr
executableAddresses puzzle =
    puzzle.layout
        |> Dict.filter (\_ v -> v == Executable)
        |> Dict.keys


inputAddr : IOConfig -> Addr
inputAddr { x } =
    ( x, 0 )


outputAddr : IOConfig -> Addr
outputAddr { x } =
    ( x, 4 )


addrAboveOutput : IOConfig -> Addr
addrAboveOutput c =
    outputAddr c |> U.moveInDir4 Up
