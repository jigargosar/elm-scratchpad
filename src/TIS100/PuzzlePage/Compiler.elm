module TIS100.PuzzlePage.Compiler exposing (..)

import Parser.Advanced as Parser
    exposing
        ( (|.)
        , (|=)
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


type alias Context =
    ()


type Problem
    = ExpectingStmtEnd
    | ExpectingComment
    | ExpectingOpVar
    | InvalidOpVarFound String
    | ExpectingLabelVar
    | ExpectingLabelSep


type alias DeadEnd =
    Parser.DeadEnd Context Problem


type alias DeadEnds =
    List DeadEnd


compileRaw : String -> Result DeadEnds Stmt
compileRaw string =
    run stmt (string ++ "\n")


compile =
    compileRaw


type Stmt
    = OnlyLabel String
    | OnlyInst Inst
    | LabelInst String Inst


stmt : Parser Stmt
stmt =
    oneOf
        [ labelVariable
            |> andThen labelSep
            |> andThen labeledStmt
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
    instOpVariable
        |> andThen
            (\opVarName ->
                instBody opVarName
                    |> andThen instEnd
            )


instEnd : Inst -> Parser Inst
instEnd i =
    succeed i
        |. spaceChars
        |. stmtEnd


instOpVariable : Parser String
instOpVariable =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlpha c
        , reserved = Set.empty
        , expecting = ExpectingOpVar
        }


instBody : String -> Parser Inst
instBody opVarName =
    case opVarName of
        "mov" ->
            succeed IMov

        "nop" ->
            succeed INop

        _ ->
            problem (InvalidOpVarFound opVarName)


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


labelSep : String -> Parser String
labelSep labelValue =
    succeed labelValue
        |. spaceChars
        |. symbol (Token ":" ExpectingLabelSep)
        |. spaceChars


labelVariable : Parser String
labelVariable =
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
