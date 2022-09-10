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


type Context
    = CLabelDef String
    | CAfterInst
    | COp String


type Problem
    = ExpectingStmtEnd
    | ExpectingComment
    | ExpectingOp
    | ExpectingLabelVar
    | ExpectingLabelSep


type alias DeadEnd =
    Parser.DeadEnd Context Problem


type alias DeadEnds =
    List DeadEnd


rawCompile : String -> Result DeadEnds Stmt
rawCompile string =
    run stmt (string ++ "\n")


compile : String -> Result Error Stmt
compile =
    rawCompile >> Result.mapError toError


type Error
    = UnexpectedProblem DeadEnd
    | EmptyError
    | TooManyErrors DeadEnds
    | InvalidOp String


toError : DeadEnds -> Error
toError deadEnds =
    case deadEnds of
        [] ->
            EmptyError

        de :: [] ->
            case de.problem of
                ExpectingStmtEnd ->
                    UnexpectedProblem de

                ExpectingComment ->
                    UnexpectedProblem de

                ExpectingOp ->
                    case de.contextStack of
                        { context } :: _ ->
                            case context of
                                COp opName ->
                                    InvalidOp opName

                                _ ->
                                    UnexpectedProblem de

                        _ ->
                            UnexpectedProblem de

                ExpectingLabelVar ->
                    UnexpectedProblem de

                ExpectingLabelSep ->
                    case de.contextStack of
                        { context } :: _ ->
                            case context of
                                CLabelDef label ->
                                    InvalidOp label

                                _ ->
                                    UnexpectedProblem de

                        _ ->
                            UnexpectedProblem de

        _ ->
            TooManyErrors deadEnds


type Stmt
    = OnlyLabel String
    | OnlyInst Inst
    | LabelInst String Inst


stmt : Parser Stmt
stmt =
    oneOf
        [ labelDef |> andThen labeledStmt
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



--inst_ : Parser Inst
--inst_ =
--    inContext CInst <|
--        oneOf
--            [ succeed IMov
--                |. keyword (Token "mov" ExpectingOp)
--            , succeed INop
--                |. keyword (Token "nop" ExpectingOp)
--            ]
--            |. spaceChars
--            |. stmtEnd
--


inst : Parser Inst
inst =
    opVariable
        |> andThen
            (\opVarName ->
                instBody opVarName
                    |> andThen instEnd
            )


instEnd : Inst -> Parser Inst
instEnd i =
    inContext CAfterInst <|
        succeed i
            |. spaceChars
            |. stmtEnd


instBody : String -> Parser Inst
instBody opVarName =
    inContext (COp opVarName) <|
        case opVarName of
            "mov" ->
                succeed IMov

            "nop" ->
                succeed INop

            _ ->
                problem ExpectingOp


opVariable : Parser String
opVariable =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlpha c
        , reserved = Set.empty
        , expecting = ExpectingOp
        }


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
    inContext (CLabelDef labelValue) <|
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
