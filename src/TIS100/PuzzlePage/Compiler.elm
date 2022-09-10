module TIS100.PuzzlePage.Compiler exposing (..)

import Parser.Advanced as Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Nestable(..)
        , Step(..)
        , Token(..)
        , Trailing(..)
        , andThen
        , backtrackable
        , chompIf
        , chompUntil
        , chompUntilEndOr
        , chompWhile
        , commit
        , end
        , float
        , getChompedString
        , getCol
        , getIndent
        , getOffset
        , getPosition
        , getRow
        , getSource
        , inContext
        , int
        , keyword
        , lazy
        , lineComment
        , loop
        , map
        , mapChompedString
        , multiComment
        , number
        , oneOf
        , problem
        , run
        , sequence
        , spaces
        , succeed
        , symbol
        , token
        , variable
        , withIndent
        )
import Set exposing (Set)


type alias Parser x =
    Parser.Parser Context Problem x


type Context
    = CLabelDef
    | CInst


type Problem
    = ExpectingStmtEnd
    | ExpectingComment
    | ExpectingOp
    | ExpectingLabelVar
    | ExpectingLabelSep


compile : String -> Result (List (DeadEnd Context Problem)) Stmt
compile string =
    run stmt (string ++ "\n")


type Stmt
    = OnlyLabel String
    | OnlyInst Inst
    | LabelInst String Inst


stmt : Parser Stmt
stmt =
    oneOf
        [ labelDef
            |> andThen
                labeledStmt
        , succeed OnlyInst
            |= inst
        ]


labeledStmt : String -> Parser Stmt
labeledStmt labelValue =
    oneOf
        [ succeed (LabelInst labelValue)
            |= inst
        , succeed (OnlyLabel labelValue)
            |. stmtEnd
        ]


stmtEnd : Parser ()
stmtEnd =
    succeed ()
        |. spaces
        |. oneOf
            [ lineComment (Token "#" ExpectingComment)
            , succeed ()
            ]
        |. symbol (Token "\n" ExpectingStmtEnd)


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ')


inst : Parser Inst
inst =
    inContext CInst <|
        oneOf
            [ succeed IMov
                |. keyword (Token "mov" ExpectingOp)
            , succeed INop
                |. keyword (Token "nop" ExpectingOp)
            ]
            |. spaceChars
            |. stmtEnd


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


labelDef : Parser String
labelDef =
    labelName |> andThen labelSep


labelSep : String -> Parser String
labelSep labelValue =
    inContext CLabelDef <|
        succeed labelValue
            |. spaceChars
            |. symbol (Token ":" ExpectingLabelSep)
            |. spaceChars


labelName : Parser String
labelName =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = opNames
        , expecting = ExpectingLabelVar
        }


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
