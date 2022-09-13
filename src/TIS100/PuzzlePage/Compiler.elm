module TIS100.PuzzlePage.Compiler exposing (..)

import Parser.Advanced as Parser exposing (..)
import Set exposing (Set)
import TIS100.Num as Num exposing (Num)
import Utils exposing (Dir4(..))


type alias Parser x =
    Parser.Parser Context ProblemPrivate x


type alias Context =
    ()


type ProblemPrivate
    = Expecting String
    | ReportAs Problem


type Problem
    = InvalidOp
    | InvalidSrc
    | InvalidDst
    | TooManyArgs


type alias DeadEnd =
    Parser.DeadEnd Context ProblemPrivate


compileStmt : String -> Result Error Stmt
compileStmt string =
    run stmtParser (string ++ "\n")
        |> Result.mapError deadEndsToError


type alias Error =
    { col : Int, problem : Problem }


deadEndsToError : List DeadEnd -> Error
deadEndsToError deadEnds =
    case deadEnds of
        d :: [] ->
            case d.problem of
                ReportAs error ->
                    Error d.col error

                Expecting _ ->
                    Debug.todo (Debug.toString ( "internal error", deadEnds ))

        _ ->
            Debug.todo (Debug.toString ( "internal error", deadEnds ))


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
        |. stmtEnd (Expecting "OnlyLabelStmt")
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
        |. spaces
        |. symbol ":"
        |. spaces


stmtEnd : ProblemPrivate -> Parser ()
stmtEnd problem =
    succeed ()
        |. spaces
        |. oneOf
            [ lineComment (toToken "#")
            , succeed ()
            ]
        |. Parser.symbol (Token "\n" problem)


instParser : Parser Inst
instParser =
    opParser
        |> andThen
            (\op ->
                instBody op
                    |. spaces
                    |. stmtEnd (ReportAs TooManyArgs)
            )


instBody : Op -> Parser Inst
instBody opVarName =
    case opVarName of
        Op_Mov ->
            succeed Mov
                |. spaces
                |= srcParser
                |. spaces
                |= dstParser

        Op_Nop ->
            succeed Nop


srcParser : Parser Src
srcParser =
    oneOfWithSingleProblem (ReportAs InvalidSrc)
        [ numParser |> map SrcNum
        , dirParser |> map SrcPort
        ]


dstParser : Parser Dst
dstParser =
    oneOfWithSingleProblem (ReportAs InvalidDst)
        [ succeed DstAcc |. keyword "acc"
        ]


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
    oneOfWithSingleProblem (ReportAs InvalidOp)
        [ succeed Op_Mov |. keyword "mov"
        , succeed Op_Nop |. keyword "nop"
        ]


dirParser : Parser Dir4
dirParser =
    oneOf
        [ succeed Up |. keyword "up"
        , succeed Down |. keyword "down"
        , succeed Left |. keyword "left"
        , succeed Right |. keyword "right"
        ]


numParser : Parser Num
numParser =
    succeed Num.fromInt
        |= oneOf
            [ succeed negate
                |. symbol "-"
                |= int (Expecting "a Num") (Expecting "a Num")
            , int (Expecting "a Num") (Expecting "a Num")
            ]


labelVariable : Parser String
labelVariable =
    variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = opNames --  |> always Set.empty
        , expecting = Expecting "a LabelVar"
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


symbol : String -> Parser ()
symbol str =
    Parser.symbol (Token str (Expecting str))


keyword : String -> Parser ()
keyword str =
    Parser.keyword (Token str (Expecting str))


type alias Token =
    Parser.Token ProblemPrivate


toToken : String -> Token
toToken str =
    Token str (Expecting str)


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ')
