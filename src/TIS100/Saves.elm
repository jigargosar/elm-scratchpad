module TIS100.Saves exposing (Saves, decoder, encode, fromList, get, initial, set)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import TIS100.Addr exposing (Addr)
import TIS100.Puzzle as Puzzle
import Utils exposing (mapFirst, pair)


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


pairDecoder : Decoder a -> Decoder b -> Decoder ( a, b )
pairDecoder da db =
    JD.map2 pair
        (JD.index 0 da)
        (JD.index 1 db)


encode : Saves -> Value
encode (Saves dict) =
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

        encodeKey : String -> String
        encodeKey key =
            "tis100.saves." ++ key
    in
    JE.dict encodeKey encodeSrcEntries dict


decoder : Decoder Saves
decoder =
    let
        srcEntriesDecoder : Decoder (List ( Addr, String ))
        srcEntriesDecoder =
            JD.list srcEntryDecoder

        srcEntryDecoder : Decoder ( Addr, String )
        srcEntryDecoder =
            pairDecoder addrDecoder JD.string

        addrDecoder : Decoder Addr
        addrDecoder =
            pairDecoder JD.int JD.int

        fromDict : Dict String (List ( Addr, String )) -> Saves
        fromDict dict =
            dict
                |> Dict.toList
                |> List.filterMap
                    (\( k, v ) ->
                        if String.startsWith "tis100.saves." k then
                            Just ( String.replace "tis100.saves." "" k, v )

                        else
                            Nothing
                    )
                |> Dict.fromList
                |> Saves
    in
    JD.dict srcEntriesDecoder
        |> JD.map fromDict


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
