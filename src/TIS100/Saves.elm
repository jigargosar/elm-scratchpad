module TIS100.Saves exposing (Saves, fromList, get, initial, set)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value)
import Json.Encode as JE
import TIS100.Addr exposing (Addr)
import TIS100.Puzzle as Puzzle
import Utils exposing (mapFirst)


type Saves
    = Saves (Dict String (List ( Addr, String )))


initial : Saves
initial =
    fromList
        [ ( Puzzle.SignalComparator, signalComparatorSourceEntries )
        , ( Puzzle.Sample, sampleSourceEntries )
        ]


encodePair : (a -> Value) -> (b -> Value) -> ( a, b ) -> Value
encodePair fa fb ( a, b ) =
    JE.list identity [ fa a, fb b ]


encodeSaves : Saves -> Value
encodeSaves (Saves dict) =
    let
        encodeSrcEntries : List ( Addr, String ) -> JE.Value
        encodeSrcEntries =
            JE.list encodeSrcEntry

        encodeSrcEntry : ( Addr, String ) -> Value
        encodeSrcEntry =
            encodePair encodeAddr JE.string

        encodeAddr : Addr -> Value
        encodeAddr =
            encodePair JE.int JE.int
    in
    JE.dict identity encodeSrcEntries dict


sampleSourceEntries : List ( Addr, String )
sampleSourceEntries =
    [ ( ( 0, 1 ), "mov up acc\n\n\nmov acc down" )
    , ( ( 0, 2 ), "Mov up down\nmov 1 acc" )
    , ( ( 0, 3 ), "Mov up down\nnop" )
    , ( ( 1, 1 ), "Mov up down" )
    , ( ( 1, 2 ), "Mov up down" )
    , ( ( 1, 3 ), "Mov up down" )
    , ( ( 2, 1 ), "lbl:Jmp lbl\n jmp : \na : Jmp : " )
    , ( ( 2, 2 ), "Mov up down" )
    , ( ( 2, 3 ), "Mov up down" )
    , ( ( 3, 1 ), "Mov up down" )
    , ( ( 3, 2 ), "Mov up down" )
    , ( ( 3, 3 ), "Mov up down" )
    ]


signalComparatorSourceEntries : List ( Addr, String )
signalComparatorSourceEntries =
    [ ( ( 0, 1 ), "MOV UP DOWN" )
    , ( ( 0, 2 ), "MOV UP DOWN" )
    , ( ( 0, 3 ), "MOV UP right" )
    , ( ( 1, 1 ), "" )
    , ( ( 1, 2 ), "" )
    , ( ( 1, 3 )
      , [ "S: MOV LEFT ACC"
        , "MOV ACC RIGHT"
        , ""
        , "JGZ 1"
        , "MOV 0 DOWN"
        , "JMP S"
        , ""
        , "1: MOV 1 DOWN"
        ]
            |> String.join "\n"
      )
    , ( ( 2, 1 ), "" )
    , ( ( 2, 2 ), "" )
    , ( ( 2, 3 )
      , [ "S: MOV LEFT ACC"
        , "MOV ACC RIGHT"
        , ""
        , "JEZ 1"
        , "MOV 0 DOWN"
        , "JMP S"
        , ""
        , "1: MOV 1 DOWN"
        ]
            |> String.join "\n"
      )
    , ( ( 3, 1 ), "" )
    , ( ( 3, 2 ), "" )
    , ( ( 3, 3 )
      , [ "S: MOV LEFT ACC"
        , "# MOV ACC RIGHT"
        , ""
        , "JLZ 1"
        , "MOV 0 DOWN"
        , "JMP S"
        , ""
        , "1: MOV 1 DOWN"
        ]
            |> String.join "\n"
      )
    ]


fromList : List ( Puzzle.Id, List ( Addr, String ) ) -> Saves
fromList ls =
    Saves (ls |> List.map (mapFirst idToString) |> Dict.fromList)


idToString : Puzzle.Id -> String
idToString name =
    case name of
        Puzzle.SignalComparator ->
            "SignalComparator"

        Puzzle.Sample ->
            "Sample"


set : Puzzle.Id -> List ( Addr, String ) -> Saves -> Saves
set name srcEntries (Saves dict) =
    Saves (Dict.insert (idToString name) srcEntries dict)


get : Puzzle.Id -> Saves -> List ( Addr, String )
get name (Saves dict) =
    Dict.get (idToString name) dict
        |> Maybe.withDefault []
