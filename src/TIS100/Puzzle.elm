module TIS100.Puzzle exposing
    ( InConfig
    , Name(..)
    , OutConfig
    , Puzzle
    , description
    , fromName
    , getExeAddr
    , inNums
    , inTitle
    , inX
    , leftBarViewModel
    , samplePuzzle1
    , signalComparator
    , title
    , toDictBy
    , toListBy
    , validWrites
    )

import Dict exposing (Dict)
import Random
import TIS100.Addr exposing (Addr)
import TIS100.Num as Num exposing (Num)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.SelectionList as SelectionList
import Utils as U exposing (Dir4(..), pair, unzip3)


type Name
    = SignalComparator


fromName : Name -> Puzzle
fromName puzzleName =
    case puzzleName of
        SignalComparator ->
            signalComparator


type Puzzle
    = Puzzle
        { title : String
        , description : List String
        , inputs : List InConfig
        , outputs : List OutConfig
        , layout : Layout
        }


type alias Layout =
    Dict Addr NodeType


type InConfig
    = InConfig
        { x : Int
        , title : String
        , nums : List Num
        }


type alias OutConfig =
    { x : Int
    , title : String
    , nums : List Num
    }


type NodeType
    = Exe
    | Flt


samplePuzzle1 : Puzzle
samplePuzzle1 =
    Puzzle
        { title = "Differential Converter"
        , description =
            [ "READ VALUES FROM IN.A AND IN.B"
            , "WRITE IN.A - IN.B TO OUT.P"
            , "WRITE IN.B - IN.A TO OUT.N"
            ]
        , inputs =
            [ InConfig { x = 0, title = "IN.A", nums = Num.range 1 20 }
            , InConfig { x = 1, title = "IN.B", nums = Num.range 1 20 }
            ]
        , outputs =
            [ { x = 0, title = "OUT.P", nums = Num.range 1 20 }
            , { x = 1, title = "OUT.N", nums = Num.range 1 20 }
            ]
        , layout =
            [ [ Exe, Exe, Exe, Exe ]
            , [ Exe, Exe, Flt, Exe ]
            , [ Exe, Exe, Exe, Exe ]
            ]
                |> toLayout
        }


signalComparator : Puzzle
signalComparator =
    let
        rIn =
            Random.int -2 2

        rInList =
            Random.list 39 rIn

        is =
            U.stepWithInitialSeed 0 rInList

        os =
            List.map
                (\i ->
                    case compare i 0 of
                        GT ->
                            ( 1, 0, 0 )

                        EQ ->
                            ( 0, 1, 0 )

                        LT ->
                            ( 0, 0, 1 )
                )
                is

        ( gs, es, ls ) =
            unzip3 os
    in
    Puzzle
        { title = "signal comparator"
        , description =
            [ "READ VALUES FROM IN"
            , "WRITE 1 TO OUT.G IF IN > 0"
            , "WRITE 1 TO OUT.E IF IN = 0"
            , "WRITE 1 TO OUT.L IF IN < 0"
            , "WHEN A 1 IS NOT WRITTEN TO AN OUTPUT, WRITE A 0 INSTEAD"

            --, "WHEN A 1 IS NOT WRITTEN TO"
            --, "AN OUTPUT, WRITE A 0 INSTEAD"
            ]
        , inputs =
            [ InConfig { x = 0, title = "IN", nums = Num.fromList is }
            ]
        , outputs =
            [ { x = 1, title = "OUT.G", nums = Num.fromList gs }
            , { x = 2, title = "OUT.E", nums = Num.fromList es }
            , { x = 3, title = "OUT.L", nums = Num.fromList ls }
            ]
        , layout =
            [ [ Exe, Exe, Exe, Exe ]
            , [ Exe, Flt, Flt, Flt ]
            , [ Exe, Exe, Exe, Exe ]
            ]
                |> toLayout
        }


toLayout : List (List NodeType) -> Layout
toLayout lss =
    List.concat lss
        ++ List.repeat 12 Exe
        |> List.take 12
        |> List.indexedMap pair
        |> List.map
            (\( i, nk ) ->
                ( ( modBy 4 i, i // 4 + 1 ), nk )
            )
        |> Dict.fromList


leftBarViewModel : Puzzle -> LB.ViewModel
leftBarViewModel (Puzzle puzzle) =
    { title = puzzle.title
    , description = puzzle.description
    , inputs = List.map toLBInput puzzle.inputs
    , outputs = List.map toLBOutput puzzle.outputs
    }


title (Puzzle puzzle) =
    puzzle.title


description (Puzzle puzzle) =
    puzzle.description


toLBInput : InConfig -> LB.Input
toLBInput (InConfig conf) =
    { title = conf.title
    , nums = SelectionList.None conf.nums
    }


toLBOutput : OutConfig -> LB.Output
toLBOutput conf =
    { title = conf.title
    , expected = SelectionList.None conf.nums
    , actual = []
    }


isLayoutAddress : Addr -> Bool
isLayoutAddress ( x, y ) =
    U.isBounded 0 3 x && U.isBounded 1 3 y


validWrites : Puzzle -> List ( Addr, Dir4 )
validWrites puzzle =
    toListBy
        { in_ = \c -> [ ( inputAddr c, Down ) ]
        , out = \c -> [ ( addrAboveOutput c, Down ) ]
        , exe = \addr -> List.map (pair addr) (validWriteDirs addr)
        , flt = always []
        }
        puzzle
        |> List.concat


validWriteDirs : Addr -> List Dir4
validWriteDirs addr =
    let
        isValid dir =
            isLayoutAddress (U.moveInDir4 dir addr)
    in
    List.filter isValid [ Up, Down, Left, Right ]


toDictBy :
    { in_ : InConfig -> v
    , out : OutConfig -> v
    , exe : Addr -> v
    , flt : Addr -> v
    }
    -> Puzzle
    -> Dict Addr v
toDictBy { in_, out, exe, flt } (Puzzle puzzle) =
    let
        io =
            List.map (\c -> ( inputAddr c, in_ c )) puzzle.inputs
                ++ List.map (\c -> ( outputAddr c, out c )) puzzle.outputs
                |> Dict.fromList

        layout =
            Dict.map
                (\a nt ->
                    case nt of
                        Exe ->
                            exe a

                        Flt ->
                            flt a
                )
                puzzle.layout
    in
    Dict.union io layout


toListBy :
    { in_ : InConfig -> v
    , out : OutConfig -> v
    , exe : Addr -> v
    , flt : Addr -> v
    }
    -> Puzzle
    -> List v
toListBy c p =
    toDictBy c p |> Dict.values


getExeAddr : Puzzle -> List Addr
getExeAddr (Puzzle puzzle) =
    puzzle.layout
        |> Dict.filter (\_ v -> v == Exe)
        |> Dict.keys


inNums : InConfig -> List Num
inNums (InConfig { nums }) =
    nums


inTitle : InConfig -> String
inTitle (InConfig inConfig) =
    inConfig.title


inputAddr : InConfig -> Addr
inputAddr (InConfig { x }) =
    ( x, 0 )


inX : InConfig -> Int
inX (InConfig { x }) =
    x


outputAddr : OutConfig -> Addr
outputAddr { x } =
    ( x, 4 )


addrAboveOutput : OutConfig -> Addr
addrAboveOutput c =
    outputAddr c |> U.moveInDir4 Up
