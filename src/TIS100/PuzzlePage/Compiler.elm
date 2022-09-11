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
import TIS100.Num as Num exposing (Num)


type alias Parser x =
    Parser.Parser Context Problem x


type alias Context =
    ()


type Problem
    = ExpectingNegativeSign
    | ExpectingInteger
    | ExpectingStmtEnd
    | ExpectingComment
    | ExpectingOp
    | ExpectingLabelVar
    | ExpectingLabelSep
    | InvalidNumber
    | InvalidOp
    | TooManyArgs


type alias DeadEnd =
    Parser.DeadEnd Context Problem


type alias DeadEnds =
    List DeadEnd


compile : String -> Result DeadEnds Stmt
compile string =
    run stmt (string ++ "\n")


type Stmt
    = OnlyLabel String
    | OnlyInst Inst
    | LabelInst String Inst


stmt : Parser Stmt
stmt =
    maybePrefixLabel
        |> andThen
            (\mbLabel ->
                case mbLabel of
                    Nothing ->
                        succeed OnlyInst
                            |= inst

                    Just l ->
                        maybeOnlyLabel l
                            |> andThen
                                (\mbStmt ->
                                    case mbStmt of
                                        Just s ->
                                            succeed s

                                        Nothing ->
                                            succeed (LabelInst l)
                                                |= inst
                                )
            )


maybeOnlyLabel : String -> Parser (Maybe Stmt)
maybeOnlyLabel l =
    oneOf
        [ succeed (OnlyLabel l)
            |. stmtEnd ExpectingStmtEnd
            |> map Just
        , succeed Nothing
        ]


maybePrefixLabel : Parser (Maybe String)
maybePrefixLabel =
    oneOf
        [ backtrackable (prefixLabel |> map Just)
        , succeed Nothing
        ]


prefixLabel : Parser String
prefixLabel =
    labelVariable
        |. spaceChars
        |. symbol (Token ":" ExpectingLabelSep)
        |. spaceChars


stmtEnd : Problem -> Parser ()
stmtEnd problem =
    succeed ()
        |. spaces
        |. oneOf
            [ lineComment (Token "#" ExpectingComment)
            , succeed ()
            ]
        |. symbol (Token "\n" problem)


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ')


inst : Parser Inst
inst =
    opParser
        |> andThen
            (\op ->
                instBody op
                    |. spaceChars
                    |. stmtEnd TooManyArgs
            )


instBody : Op -> Parser Inst
instBody opVarName =
    case opVarName of
        Op_Mov ->
            succeed Mov
                |. spaceChars
                |= numParser

        Op_Nop ->
            succeed Nop


spaceChars : Parser ()
spaceChars =
    Parser.chompWhile (\c -> c == ' ')


type Inst
    = Mov Num
    | Nop


type Op
    = Op_Mov
    | Op_Nop


opParser : Parser Op
opParser =
    oneOfWithSingleProblem InvalidOp
        [ succeed Op_Mov |. keyword (Token "mov" ExpectingOp)
        , succeed Op_Nop |. keyword (Token "nop" ExpectingOp)
        ]


numParser : Parser Num
numParser =
    succeed Num.fromInt
        |= oneOf
            [ succeed negate
                |. symbol (Token "-" ExpectingNegativeSign)
                |= int ExpectingInteger InvalidNumber
            , int ExpectingInteger InvalidNumber
            ]


labelVariable : Parser String
labelVariable =
    variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = opNames |> always Set.empty
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



-- PARSER UTILS


oneOfWithSingleProblem : a -> List (Parser.Parser c a b) -> Parser.Parser c a b
oneOfWithSingleProblem p xs =
    oneOf [ oneOf xs |> map Just, succeed Nothing ]
        |> andThen
            (\mb ->
                case mb of
                    Just v ->
                        succeed v

                    Nothing ->
                        problem p
            )



--noinspection ElmUnusedSymbol


orElse : Parser.Parser c x a -> Parser.Parser c x a -> Parser.Parser c x a
orElse b a =
    oneOf [ a |> map Just, succeed Nothing ]
        |> andThen
            (\mb ->
                case mb of
                    Just v ->
                        succeed v

                    Nothing ->
                        b
            )
