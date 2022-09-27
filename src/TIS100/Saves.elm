module TIS100.Saves exposing (Saves, decoder, encode, get, set)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import TIS100.Addr exposing (Addr)
import TIS100.Puzzle as Puzzle
import Utils exposing (mapFirst, pair, pairTo)


type Saves
    = Saves (Dict String (List ( Addr, String )))


fromList : List ( Puzzle.Id, List ( Addr, String ) ) -> Saves
fromList newList =
    let
        initialDict : Dict String (List ( Addr, String ))
        initialDict =
            [ ( Puzzle.SignalComparator, signalComparatorSourceEntries )
            , ( Puzzle.Sample, sampleSourceEntries )
            ]
                |> List.map (mapFirst encodePuzzleId)
                |> Dict.fromList

        newDict =
            newList
                |> List.map (mapFirst encodePuzzleId)
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

        parseKeys : Dict String (List ( Addr, String )) -> Saves
        parseKeys dict =
            dict
                |> Dict.toList
                |> List.filterMap
                    (\( k, v ) ->
                        JD.decodeString puzzleIdDecoder k
                            |> Result.toMaybe
                            |> Maybe.map (pairTo v)
                    )
                |> fromList
    in
    JD.dict srcEntriesDecoder
        |> JD.map parseKeys


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


encodePuzzleId : Puzzle.Id -> String
encodePuzzleId name =
    case name of
        Puzzle.SignalComparator ->
            "SignalComparator"

        Puzzle.Sample ->
            "Sample"


puzzleIdDecoder : Decoder Puzzle.Id
puzzleIdDecoder =
    JD.andThen
        (\str ->
            case str of
                "SignalComparator" ->
                    JD.succeed Puzzle.SignalComparator

                "Sample" ->
                    JD.succeed Puzzle.Sample

                _ ->
                    JD.fail ("Invalid Puzzle Id: " ++ str)
        )
        JD.string


set : Puzzle.Id -> List ( Addr, String ) -> Saves -> Saves
set name srcEntries (Saves dict) =
    Saves (Dict.insert (encodePuzzleId name) srcEntries dict)


get : Puzzle.Id -> Saves -> List ( Addr, String )
get name (Saves dict) =
    Dict.get (encodePuzzleId name) dict
        |> Maybe.withDefault []
