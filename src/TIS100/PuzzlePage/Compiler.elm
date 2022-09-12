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
    = ExpectingNegativeSign
    | ExpectingInteger
    | ExpectingStmtEnd
    | ExpectingComment
    | ExpectingOp
    | ExpectingLabelVar
    | ExpectingLabelSep
    | ExpectingAcc
    | InvalidNumber
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
    | LabelInst String Inst
    | LabeledStmt String (Maybe Inst)


stmtToString : Stmt -> String
stmtToString stmt =
    case stmt of
        OnlyLabel label ->
            label ++ ":"

        OnlyInst inst ->
            instToString inst

        LabelInst label inst ->
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


stmtParser2 : Parser Stmt
stmtParser2 =
    oneOf
        [ labeledStatementParser
        , onlyInstStatementParser
        ]


labeledStatementParser : Parser Stmt
labeledStatementParser =
    let
        isColon : Char -> Bool
        isColon =
            \c -> c == ':'

        parsePrefixLabelIfAny : Parser String
        parsePrefixLabelIfAny =
            backtrackable labelVariable
                |. spaceChars
                |. chompIf isColon ExpectingLabelSep
                |. spaceChars
    in
    Debug.todo "todo"


onlyInstStatementParser : Parser Stmt
onlyInstStatementParser =
    Debug.todo "todo"


stmtParser : Parser Stmt
stmtParser =
    maybePrefixLabel
        |> andThen
            (\mbLabel ->
                case mbLabel of
                    Nothing ->
                        succeed OnlyInst
                            |= instParser

                    Just l ->
                        maybeOnlyLabel l
                            |> andThen
                                (\mbStmt ->
                                    case mbStmt of
                                        Just s ->
                                            succeed s

                                        Nothing ->
                                            succeed (LabelInst l)
                                                |= instParser
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
                |= int ExpectingInteger InvalidNumber
            , int ExpectingInteger InvalidNumber
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
