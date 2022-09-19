module TIS100.PuzzlePage.Compiler exposing
    ( Error(..)
    , ErrorDetail
    , Errors
    , compile
    , compileLine
    , errorsToDetails
    , getErrorDetails
    , lexLine
    , prefixLabelToken
    , wordToken
    )

import List.Extra
import Parser exposing (..)
import Set exposing (Set)
import TIS100.Num as Num exposing (Num)
import TIS100.PuzzlePage.Inst exposing (..)
import Utils as U exposing (Dir4(..))


type Error
    = InvalidOpCode Int String
    | InvalidExpression Int String
    | MissingOperand Int Int
    | TooManyOperands Int Int
    | InternalError
    | UndefinedLabel Int String


type alias ErrorDetail =
    { row : Int
    , startCol : Int
    , endCol : Int
    , msg : String
    }


type alias Errors =
    List ( Int, Error )


errorsToDetails : Errors -> List ErrorDetail
errorsToDetails =
    List.filterMap
        (\( l, e ) ->
            case e of
                InvalidOpCode startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "Invalid op:\"" ++ string ++ "\""
                        }

                InvalidExpression startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "Invalid op:\"" ++ string ++ "\""
                        }

                UndefinedLabel startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "undefined label"
                        }

                MissingOperand startCol endCol ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = endCol
                        , msg = "missing operand"
                        }

                TooManyOperands startCol endCol ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = endCol
                        , msg = "too many operands"
                        }

                InternalError ->
                    Nothing
        )


getErrorDetails : String -> List ErrorDetail
getErrorDetails srcCode =
    case compile srcCode of
        Err errors ->
            errorsToDetails errors

        Ok _ ->
            []


type alias PLine =
    ( Int, Set String, Inst )


type alias Prg =
    List PLine


compile : String -> Result Errors (List PLine)
compile string =
    string
        |> String.split "\n"
        |> List.indexedMap U.pair
        |> List.map (U.biMap U.inc compileLine)
        |> List.foldr
            (\( row, result ) ( errAcc, okAcc ) ->
                case result of
                    Err err ->
                        ( ( row, err ) :: errAcc, okAcc )

                    Ok v ->
                        ( errAcc, ( row, v ) :: okAcc )
            )
            ( [], [] )
        |> (\( es, os ) ->
                case ( es, toPrg os ) of
                    ( [], Ok prg ) ->
                        Ok prg

                    ( _, Ok _ ) ->
                        Err es

                    ( _, Err nes ) ->
                        Err <| es ++ nes
           )


toPrg : List ( Int, Stmt ) -> Result Errors (List PLine)
toPrg stmts =
    case labelErrors stmts of
        [] ->
            Ok (toPLines stmts)

        es ->
            Err es


labelErrors : List ( Int, Stmt ) -> Errors
labelErrors =
    let
        step _ acc =
            acc

        done _ =
            []
    in
    List.foldl step
        { errors = []
        , prefixLabels = Set.empty
        }
        >> done


toPLines : List ( Int, Stmt ) -> List PLine
toPLines =
    let
        insertMaybeLabel mbl =
            case mbl of
                Just ( lbl, _ ) ->
                    Set.insert lbl

                _ ->
                    identity

        step ( row, stmt ) acc =
            case stmt of
                Stmt mbl (Just inst) ->
                    { acc
                        | revPLines =
                            ( row
                            , insertMaybeLabel mbl acc.prevLabels
                            , inst
                            )
                                :: acc.revPLines
                        , prevLabels = Set.empty
                    }

                Stmt mbl Nothing ->
                    { acc | prevLabels = insertMaybeLabel mbl acc.prevLabels }

        done acc =
            acc.revPLines
                |> List.reverse
                |> mapHead
                    (\( row, labels, inst ) ->
                        ( row, Set.union acc.prevLabels labels, inst )
                    )
    in
    List.foldl step { prevLabels = Set.empty, revPLines = [] }
        >> done


mapHead : (a -> a) -> List a -> List a
mapHead fn xs =
    case xs of
        [] ->
            []

        h :: t ->
            fn h :: t


compileLine : String -> Result Error Stmt
compileLine src =
    case lexLine src of
        Ok tokens ->
            parseLine tokens

        Err _ ->
            Err InternalError


type Stmt
    = Stmt (Maybe ( String, Loc )) (Maybe Inst)


stmtWithLabel : String -> Loc -> Maybe Inst -> Stmt
stmtWithLabel string loc =
    Stmt (Just ( string, loc ))


stmtWithoutLabel : Maybe Inst -> Stmt
stmtWithoutLabel =
    Stmt Nothing


parseLine : List Token -> Result Error Stmt
parseLine tokens =
    case tokens of
        (Token (PrefixLabel lbl) loc) :: rest ->
            parseInst rest
                |> Result.map (stmtWithLabel lbl loc)

        _ ->
            parseInst tokens
                |> Result.map stmtWithoutLabel


parseInst : List Token -> Result Error (Maybe Inst)
parseInst tokens =
    case tokens of
        [] ->
            Ok Nothing

        x :: xs ->
            parseInstHelp x xs
                |> Result.map Just


parseInstHelp : Token -> List Token -> Result Error Inst
parseInstHelp fst rest =
    case fst of
        Token (OpCode op) _ ->
            case op of
                NOP ->
                    withZeroArgOp Nop rest

                JMP ->
                    with1ArgOp (parseJumpInst Jmp) fst rest

                MOV ->
                    with2ArgOp parseMovInst fst rest

        _ ->
            invalidOp fst


parseJumpInst : (Label -> value) -> Token -> Result error value
parseJumpInst fn (Token _ (Loc col string)) =
    fn (Label { col = col, val = string }) |> Ok


parseMovInst : Token -> Token -> Result Error Inst
parseMovInst a b =
    Result.map2 Mov
        (parseSrcOperand a)
        (parseDstOperand b)


parseDstOperand : Token -> Result Error Dst
parseDstOperand ((Token typ _) as t) =
    case typ of
        DIR dir ->
            Ok <| DstPort dir

        ACC ->
            Ok <| DstAcc

        NIL ->
            Ok DstNil

        _ ->
            invalidExpr t


parseSrcOperand : Token -> Result Error Src
parseSrcOperand ((Token typ _) as t) =
    case typ of
        DIR dir ->
            Ok <| SrcPort dir

        NUM num ->
            Ok <| SrcNum num

        ACC ->
            Ok <| SrcAcc

        _ ->
            invalidExpr t


withZeroArgOp : v -> List Token -> Result Error v
withZeroArgOp v rest =
    case rest of
        [] ->
            Ok v

        x :: xs ->
            tooManyOperands x xs


with1ArgOp :
    (Token -> Result Error value)
    -> Token
    -> List Token
    -> Result Error value
with1ArgOp fn fst rest =
    case rest of
        a :: [] ->
            fn a

        [] ->
            missingOperand fst fst

        _ :: x :: xs ->
            tooManyOperands x xs


with2ArgOp :
    (Token -> Token -> Result Error value)
    -> Token
    -> List Token
    -> Result Error value
with2ArgOp fn fst rest =
    case rest of
        a :: b :: [] ->
            --parseMoveInst a b
            fn a b

        [] ->
            missingOperand fst fst

        last :: [] ->
            missingOperand fst last

        _ :: _ :: x :: xs ->
            tooManyOperands x xs


tooManyOperands : Token -> List Token -> Result Error value
tooManyOperands first rest =
    let
        last =
            List.Extra.last rest
                |> Maybe.withDefault first
    in
    Err (TooManyOperands (tokenStartColumn first) (tokenEndColumn last))


missingOperand : Token -> Token -> Result Error value
missingOperand start end =
    Err (MissingOperand (tokenStartColumn start) (tokenEndColumn end))


undefinedLabel : Token -> Result Error value
undefinedLabel (Token _ (Loc col string)) =
    Err (UndefinedLabel col string)


invalidOp : Token -> Result Error value
invalidOp (Token _ (Loc col string)) =
    Err (InvalidOpCode col string)


invalidExpr : Token -> Result Error value
invalidExpr (Token _ (Loc col string)) =
    Err (InvalidExpression col string)


tokenStartColumn : Token -> Int
tokenStartColumn token =
    case token of
        Token _ (Loc col _) ->
            col


tokenEndColumn : Token -> Int
tokenEndColumn token =
    case token of
        Token _ (Loc col string) ->
            col + String.length string - 1


lexLine : String -> Result (List DeadEnd) (List Token)
lexLine src =
    Parser.run tokenListParser (String.toLower src)


type Token
    = Token TokenTyp Loc


type Loc
    = Loc Int String


type TokenTyp
    = Word String
    | PrefixLabel String
    | OpCode OpCode
    | DIR Dir4
    | ACC
    | NIL
    | NUM Num


type OpCode
    = NOP
    | MOV
    | JMP


wordToken : Int -> String -> Token
wordToken col string =
    Token (Word string) (Loc col string)


prefixLabelToken : Int -> String -> Token
prefixLabelToken col str =
    Token (PrefixLabel str) (Loc col str)


tokenListParser : Parser (List Token)
tokenListParser =
    oneOf
        [ backtrackable prefixLabelTokenParser
            |> andThen (\t -> loop [ t ] tokenListParserHelp)
        , loop [] tokenListParserHelp
        ]


tokenListParserHelp : List Token -> Parser (Step (List Token) (List Token))
tokenListParserHelp rs =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (Loop rs)
                |. lineComment "#"
            , succeed (\t -> Loop (t :: rs))
                |= tokenParser
            , succeed ()
                |. end
                |> map (\_ -> Done <| List.reverse rs)
            ]


tokenParser : Parser Token
tokenParser =
    oneOf
        [ opCodeTokenParser
        , toTokenParser (keyword2 ACC "acc")
        , toTokenParser (keyword2 NIL "nil")
        , numTokenParser
        , dirTokenParser
        , wordTokenParser
        ]


wordTokenParser : Parser Token
wordTokenParser =
    toTokenParser
        (succeed Word
            |= variable
                { start = isWordChar
                , inner = isWordChar
                , reserved = Set.empty
                }
        )


opCodeTokenParser : Parser Token
opCodeTokenParser =
    toTokenParser
        (succeed OpCode
            |= oneOf
                [ keyword2 MOV "mov"
                , keyword2 NOP "nop"
                , keyword2 JMP "jmp"
                ]
        )


dirTokenParser : Parser Token
dirTokenParser =
    toTokenParser
        (succeed DIR
            |= oneOf
                [ keyword2 Up "up"
                , keyword2 Down "down"
                , keyword2 Left "left"
                , keyword2 Right "right"
                ]
        )


numTokenParser : Parser Token
numTokenParser =
    toTokenParser
        (succeed (Num.fromInt >> NUM)
            |= oneOf
                [ succeed negate
                    |. symbol "-"
                    |= int
                , int
                ]
        )


prefixLabelTokenParser : Parser Token
prefixLabelTokenParser =
    toTokenParser
        (succeed PrefixLabel
            |= labelVariable
        )
        |. spaces
        |. symbol ":"


labelVariable : Parser String
labelVariable =
    variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


toTokenParser : Parser TokenTyp -> Parser Token
toTokenParser ttp =
    succeed
        (\startCol startOffset tokenType endOffset src ->
            Token
                tokenType
            <|
                Loc startCol
                    (String.slice startOffset endOffset src)
        )
        |= getCol
        |= getOffset
        |= ttp
        |= getOffset
        |= getSource


keyword2 tag string =
    succeed tag |. keyword string


isWordChar : Char -> Bool
isWordChar c =
    c /= ' ' && c /= '#'
