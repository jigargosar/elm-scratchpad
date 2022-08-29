module TIS100.Puzzle exposing (..)

import TIS100.Num as Num exposing (Num)


type alias Puzzle =
    { title : String
    , description : List String
    , inputs : List ( Int, String, List Num )
    , outputs : List ( Int, String, List Num )
    , layout : List NodeType
    }


type alias IOData =
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
        [ ( 0, "IN.A", Num.range 1 20 )
        , ( 1, "IN.B", Num.range 1 20 )
        ]
    , outputs =
        [ ( 0, "OUT.P", Num.range 1 20 )
        , ( 1, "OUT.N", Num.range 1 20 )
        ]
    , layout =
        [ [ Executable, Executable, Executable, Executable ]
        , [ Executable, Executable, Executable, Executable ]
        , [ Executable, Executable, Executable, Executable ]
        ]
            |> List.concat
    }
