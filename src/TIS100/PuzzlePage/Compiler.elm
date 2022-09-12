module TIS100.PuzzlePage.Compiler exposing (..)

import Parser.Advanced as Parser exposing (..)
import Set exposing (Set)
import TIS100.Num as Num exposing (Num)
import Utils exposing (Dir4(..))


type alias Parser x =
    Parser.Parser Context Problem x


type alias Context =
    ()


type Problem
    = Expecting String
    | ExpectingNegativeSign
    | ExpectingNum
    | ExpectingOnlyLabelStmt
    | ExpectingOp
    | ExpectingLabelVar
    | ExpectingLabelSep
    | ExpectingAcc
    | InvalidOp
    | ExpectingDir
    | InvalidSrc
    | InvalidDst
    | TooManyArgs


type alias DeadEnd =
    Parser.DeadEnd Context Problem


type alias DeadEnds =
    List DeadEnd


compile : String -> Result DeadEnds Stmt
compile string =
    run stmtParser (string ++ "\n")


type Stmt
    = OnlyLabel String
    | OnlyInst Inst
    | LabeledInst String Inst
    | LabeledStmt String (Maybe Inst)


stmtToString : Stmt -> String
stmtToString stmt =
    case stmt of
        OnlyLabel label ->
            label ++ ":"

        OnlyInst inst ->
            instToString inst

        LabeledInst label inst ->
            label ++ ": " ++ instToString inst

        LabeledStmt label maybeInst ->
            (label ++ ":")
                ++ (maybeInst
                        |> Maybe.map (\inst -> " " ++ instToString inst)
                        |> Maybe.withDefault ""
                   )


instToString : Inst -> String
instToString inst =
    case inst of
        Mov src dst ->
            "mov " ++ srcToString src ++ " " ++ dstToString dst

        Nop ->
            "nop"


dstToString : Dst -> String
dstToString dst =
    case dst of
        DstAcc ->
            "acc"


srcToString : Src -> String
srcToString src =
    case src of
        SrcNum num ->
            Num.toString num

        SrcPort dir ->
            dirToString dir


dirToString : Dir4 -> String
dirToString dir =
    case dir of
        Up ->
            "up"

        Down ->
            "down"

        Left ->
            "left"

        Right ->
            "right"


stmtParser : Parser Stmt
stmtParser =
    oneOf
        [ backtrackable (prefixLabel |> map Just)
        , succeed Nothing
        ]
        |> andThen
            (Maybe.map labeledStmtParser
                >> Maybe.withDefault onlyInstStmtParser
            )


labeledStmtParser : String -> Parser Stmt
labeledStmtParser l =
    succeed (OnlyLabel l)
        |. stmtEnd ExpectingOnlyLabelStmt
        |> orElse
            (succeed (LabeledInst l)
                |= instParser
            )


onlyInstStmtParser : Parser Stmt
onlyInstStmtParser =
    succeed OnlyInst
        |= instParser


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
            [ lineComment (toToken "#")
            , succeed ()
            ]
        |. symbol (Token "\n" problem)


type alias Token =
    Parser.Token Problem


toToken : String -> Token
toToken str =
    Token str (Expecting str)


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ')


instParser : Parser Inst
instParser =
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
                |= srcParser
                |. spaceChars
                |= dstParser

        Op_Nop ->
            succeed Nop


srcParser : Parser Src
srcParser =
    oneOfWithSingleProblem InvalidSrc
        [ numParser |> map SrcNum
        , dirParser |> map SrcPort
        ]


dstParser : Parser Dst
dstParser =
    oneOfWithSingleProblem InvalidDst
        [ succeed DstAcc |. keyword (Token "acc" ExpectingAcc)
        ]


spaceChars : Parser ()
spaceChars =
    Parser.chompWhile (\c -> c == ' ')


type Inst
    = Mov Src Dst
    | Nop


type Src
    = SrcNum Num
    | SrcPort Dir4


type Dst
    = DstAcc


type Op
    = Op_Mov
    | Op_Nop


opParser : Parser Op
opParser =
    oneOfWithSingleProblem InvalidOp
        [ succeed Op_Mov |. keyword (Token "mov" ExpectingOp)
        , succeed Op_Nop |. keyword (Token "nop" ExpectingOp)
        ]


dirParser : Parser Dir4
dirParser =
    oneOf
        [ succeed Up |. keyword (Token "up" ExpectingDir)
        , succeed Down |. keyword (Token "down" ExpectingDir)
        ]


numParser : Parser Num
numParser =
    succeed Num.fromInt
        |= oneOf
            [ succeed negate
                |. symbol (Token "-" ExpectingNegativeSign)
                |= int ExpectingNum ExpectingNum
            , int ExpectingNum ExpectingNum
            ]


labelVariable : Parser String
labelVariable =
    variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = opNames --  |> always Set.empty
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
