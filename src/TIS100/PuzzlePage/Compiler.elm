module TIS100.PuzzlePage.Compiler exposing (..)

import Parser.Advanced as Parser exposing (..)
import Set exposing (Set)


type alias Parser x =
    Parser.Parser Context Problem x


type alias Context =
    ()


type Problem
    = ExpectingEnd
    | ExpectingOp
    | ExpectingLabelVar
    | ExpectingLabelSep


compile : String -> Result (List (DeadEnd Context Problem)) Stmt
compile =
    run stmt


type Stmt
    = OnlyLabel String
    | OnlyInst Inst
    | LabelInst String Inst


stmt : Parser Stmt
stmt =
    oneOf
        [ labelPrefix
            |> andThen
                (\labelValue ->
                    oneOf
                        [ succeed (LabelInst labelValue)
                            |= inst
                        , succeed (OnlyLabel labelValue)
                        ]
                )
        , succeed OnlyInst
            |= inst
        ]
        |. stmtEnd


stmtEnd : Parser ()
stmtEnd =
    succeed ()
        |. spaces
        |. oneOf
            [ end ExpectingEnd
            , symbol (Token "\n" ExpectingEnd)
            ]


inst : Parser Inst
inst =
    oneOf
        [ succeed IMov
            |. keyword (Token "mov" ExpectingOp)
        , succeed INop
            |. keyword (Token "nop" ExpectingOp)
        ]
        |. spaceChars


spaceChars : Parser ()
spaceChars =
    Parser.chompWhile (\c -> c == ' ')


type Inst
    = IMov
    | INop



--type Op
--    = Nop
--    | Mov
--    | Add
--    | Sub
--    | Sav
--    | Jmp
--    | Jez
--    | Jgz
--    | Jlz
--    | Jro
--
--opParser : Parser Op
--opParser =
--    oneOf
--        [ succeed Nop |. keyword "nop"
--        , succeed Mov |. keyword "mov"
--        , succeed Add |. keyword "add"
--        , succeed Sub |. keyword "sub"
--        , succeed Sav |. keyword "sav"
--        , succeed Jmp |. keyword "jmp"
--        , succeed Jez |. keyword "jez"
--        , succeed Jgz |. keyword "jgz"
--        , succeed Jlz |. keyword "jlz"
--        , succeed Jro |. keyword "jro"
--        ]
--        |. spaceChars
--
--num : Parser LabelOrOp
--num =
--    Parser.succeed (Num.fromInt >> Num)
--        |= Parser.oneOf
--            [ Parser.succeed negate
--                |. Parser.symbol "-"
--                |= Parser.int
--            , Parser.int
--            ]


labelPrefix : Parser String
labelPrefix =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = opNames
        , expecting = ExpectingLabelVar
        }
        |. spaceChars
        |. symbol (Token ":" ExpectingLabelSep)
        |. spaceChars


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
