module TIS100.PuzzlePage.Compiler exposing (..)

import Parser exposing (..)
import Set exposing (Set)
import TIS100.Num as Num exposing (Num)


type Token
    = Label String
    | Num Num
    | Op Op


type Op
    = Nop
    | Mov
    | Add
    | Sub
    | Sav
    | Jmp
    | Jez
    | Jgz
    | Jlz
    | Jro


token : Parser Token
token =
    oneOf [ label, num, map Op op ]


op : Parser Op
op =
    oneOf
        [ succeed Nop |. keyword "nop"
        , succeed Mov |. keyword "mov"
        , succeed Add |. keyword "add"
        , succeed Sub |. keyword "sub"
        , succeed Sav |. keyword "sav"
        , succeed Jmp |. keyword "jmp"
        , succeed Jez |. keyword "jez"
        , succeed Jgz |. keyword "jgz"
        , succeed Jlz |. keyword "jlz"
        , succeed Jro |. keyword "jro"
        ]


num : Parser Token
num =
    Parser.succeed (Num.fromInt >> Num)
        |= Parser.oneOf
            [ Parser.succeed negate
                |. Parser.symbol "-"
                |= Parser.int
            , Parser.int
            ]


label : Parser Token
label =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = opNames
        }
        |> map Label


opNames : Set String
opNames =
    Set.fromList
        [ "nop"
        , "mov"
        , "add"
        , "sub"
        , "sav"
        , "jmp"
        , "jez"
        , "jgz"
        , "jlz"
        , "jro"
        ]
