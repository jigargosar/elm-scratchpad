module TIS100.Puzzle exposing (..)

import Dict exposing (Dict)
import TIS100.Num as Num exposing (Num)
import Utils exposing (pair)


type alias Puzzle =
    { title : String
    , description : List String
    , inputs : List IOConfig
    , outputs : List IOConfig
    , layout : Layout
    }


type alias Addr =
    ( Int, Int )


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
        , [ Executable, Faulty, Faulty, Executable ]
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


gridToList :
    (IOConfig -> v)
    -> (IOConfig -> v)
    -> (( Addr, NodeType ) -> v)
    -> Puzzle
    -> List v
gridToList ifn ofn lfn puzzle =
    let
        io =
            List.map (\c -> ifn c) puzzle.inputs
                ++ List.map (\c -> ofn c) puzzle.outputs

        layout : List v
        layout =
            List.map lfn (Dict.toList puzzle.layout)
    in
    io ++ layout
