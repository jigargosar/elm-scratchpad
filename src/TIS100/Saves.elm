module TIS100.Saves exposing (Saves, decoder, encode, get, set)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import TIS100.Addr exposing (Addr)
import TIS100.Puzzle as Puzzle
import Utils exposing (filterMapFirst, mapFirst, pair)


type alias Solution =
    List ( Addr, String )


type Saves
    = Saves (Dict String Solution)


fromList : List ( Puzzle.Id, Solution ) -> Saves
fromList newList =
    let
        initialDict : Dict String Solution
        initialDict =
            [ ( Puzzle.SignalComparator, signalComparatorSolution )
            , ( Puzzle.Sample, sampleSolution )
            ]
                |> List.map (mapFirst puzzleIdToString)
                |> Dict.fromList

        newDict =
            newList
                |> List.map (mapFirst puzzleIdToString)
                |> Dict.fromList
    in
    Saves (Dict.union newDict initialDict)


encodePair : (a -> Value) -> (b -> Value) -> ( a, b ) -> Value
encodePair fa fb ( a, b ) =
    JE.list identity [ fa a, fb b ]


pairDecoder : Decoder a -> Decoder b -> Decoder ( a, b )
pairDecoder da db =
    JD.map2 pair
        (JD.index 0 da)
        (JD.index 1 db)


encode : Saves -> Value
encode (Saves dict) =
    JE.list (encodePair JE.string encodeSolution) (Dict.toList dict)


solutionDecoder : Decoder Solution
solutionDecoder =
    JD.list (pairDecoder addrDecoder JD.string)


encodeSolution : Solution -> JE.Value
encodeSolution =
    JE.list (encodePair encodeAddr JE.string)


encodeAddr : Addr -> Value
encodeAddr =
    encodePair JE.int JE.int


addrDecoder : Decoder Addr
addrDecoder =
    pairDecoder JD.int JD.int


decoder : Decoder Saves
decoder =
    JD.dict solutionDecoder
        |> JD.map
            (Dict.toList
                >> List.filterMap (filterMapFirst puzzleIdFromString)
                >> fromList
            )


sampleSolution : Solution
sampleSolution =
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


signalComparatorSolution : Solution
signalComparatorSolution =
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


puzzleIdToString : Puzzle.Id -> String
puzzleIdToString id =
    case id of
        Puzzle.SignalComparator ->
            "SignalComparator"

        Puzzle.Sample ->
            "Sample"


puzzleIdFromString : String -> Maybe Puzzle.Id
puzzleIdFromString str =
    case str of
        "SignalComparator" ->
            Just Puzzle.SignalComparator

        "Sample" ->
            Just Puzzle.Sample

        _ ->
            Nothing


set : Puzzle.Id -> Solution -> Saves -> Saves
set id solution (Saves dict) =
    Saves (Dict.insert (puzzleIdToString id) solution dict)


get : Puzzle.Id -> Saves -> Solution
get id (Saves dict) =
    Dict.get (puzzleIdToString id) dict
        |> Maybe.withDefault []
