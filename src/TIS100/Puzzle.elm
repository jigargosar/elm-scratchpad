module TIS100.Puzzle exposing
    ( InConfig
    , OutConfig
    , Puzzle
    , getExeAddr
    , inNums
    , inTitle
    , inX
    , leftBarViewModel
    , samplePuzzle
    , toDictBy
    , toListBy
    , validWrites
    )

import Dict exposing (Dict)
import TIS100.Addr exposing (Addr)
import TIS100.Num as Num exposing (Num)
import TIS100.PuzzlePage.LeftBar as LB
import TIS100.SelectionList as SelectionList
import Utils as U exposing (Dir4(..), pair)


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
    = Executable
    | Faulty


samplePuzzle : Puzzle
samplePuzzle =
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
        |> List.map
            (\( i, nk ) ->
                ( ( modBy 4 i, i // 4 + 1 ), nk )
            )
        |> Dict.fromList


leftBarViewModel : Puzzle -> LB.ViewModel
leftBarViewModel (Puzzle puzzle) =
    { inputs = List.map toLBInput puzzle.inputs
    , outputs = List.map toLBOutput puzzle.outputs
    }


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
                        Executable ->
                            exe a

                        Faulty ->
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
        |> Dict.filter (\_ v -> v == Executable)
        |> Dict.keys


inNums : InConfig -> List Num
inNums (InConfig { nums }) =
    nums


inTitle : InConfig -> String
inTitle (InConfig { title }) =
    title


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
