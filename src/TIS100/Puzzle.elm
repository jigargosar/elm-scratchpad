module TIS100.Puzzle exposing (..)

import TIS100.Num exposing (Num)


type alias Puzzle =
    { title : String
    , description : List String
    , inputs : List ( Int, String, List Num )
    , outputs : List ( Int, String, List Num )
    , nodeConfig : List NodeType
    }


type alias IOData =
    { x : Int
    , title : String
    , nums : List Num
    }


type NodeType
    = Executable
    | Faulty
