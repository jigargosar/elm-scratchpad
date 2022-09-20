module TIS100.PuzzlePage.Compiler exposing
    ( Error(..)
    , ErrorDetail
    , Errors
    , compile
    , errorsToDetails
    , getErrorDetails
    , lexLine
    , prefixLabelToken
    , wordToken
    )

import Dict exposing (Dict)
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
    | LabelAlreadyDefined Int String


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
                        , msg = "Invalid opcode \"" ++ string ++ "\""
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

                LabelAlreadyDefined startCol string ->
                    Just
                        { row = l
                        , startCol = startCol
                        , endCol = startCol + String.length string - 1
                        , msg = "label already defined"
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
    { lineNo : Int
    , labels : Set String
    , inst : Inst
    }


type alias Prg =
    List PLine


compile : String -> Result Errors (List PLine)
compile string =
    string
        |> String.split "\n"
        |> List.indexedMap U.pair
        |> List.map (U.mapFirst U.inc)
        |> (\ls -> compileLines (toLabelDefs ls) ls)


toPLines : List ( Int, Stmt ) -> List PLine
toPLines =
    let
        step ( row, stmt ) acc =
            case stmt of
                Stmt mbl (Just inst) ->
                    { acc
                        | revPLines =
                            { lineNo = row
                            , labels = U.insertMaybe mbl acc.prevLabels
                            , inst = inst
                            }
                                :: acc.revPLines
                        , prevLabels = Set.empty
                    }

                Stmt mbl Nothing ->
                    { acc | prevLabels = U.insertMaybe mbl acc.prevLabels }

        done acc =
            acc.revPLines
                |> List.reverse
                |> U.mapHead
                    (\pl ->
                        { pl | labels = Set.union acc.prevLabels pl.labels }
                    )
    in
    List.foldl step { prevLabels = Set.empty, revPLines = [] }
        >> done


type alias LabelDefs =
    Dict String Int


toLabelDefs : List ( Int, String ) -> LabelDefs
toLabelDefs =
    List.foldr
        (\( row, line ) ->
            case lexLine line of
                Ok ((Token (PrefixLabel lbl) _) :: _) ->
                    Dict.insert lbl row

                _ ->
                    identity
        )
        Dict.empty


compileLines : LabelDefs -> List ( Int, String ) -> Result Errors Prg
compileLines labelDefs =
    let
        step ( row, srcLine ) ( revErrs, revStmts ) =
            case
                srcLine
                    |> lexLine
                    |> Result.andThen (parseStmt labelDefs row)
            of
                Ok stmt ->
                    ( revErrs, ( row, stmt ) :: revStmts )

                Err err ->
                    ( ( row, err ) :: revErrs, revStmts )

        done ( revErrs, revStmts ) =
            case List.sortBy U.first (List.reverse revErrs) of
                [] ->
                    Ok (toPLines (List.reverse revStmts))

                es ->
                    Err es
    in
    List.foldl step ( [], [] )
        >> U.biMap List.reverse List.reverse
        >> done


type Stmt
    = Stmt (Maybe String) (Maybe Inst)


parseStmt : LabelDefs -> Int -> List Token -> Result Error Stmt
parseStmt labelDefs row tokens =
    case tokens of
        (Token (PrefixLabel lbl) (Loc col _)) :: otherTokens ->
            if Dict.get lbl labelDefs /= Just row then
                Err (LabelAlreadyDefined col lbl)

            else
                Result.map (Stmt (Just lbl)) (parseInst labelDefs otherTokens)

        otherTokens ->
            Result.map (Stmt Nothing) (parseInst labelDefs otherTokens)


parseInst : LabelDefs -> List Token -> Result Error (Maybe Inst)
parseInst labelDefs tokens =
    case tokens of
        [] ->
            Ok Nothing

        t :: ts ->
            Result.map Just (parseInstHelp labelDefs t ts)


parseInstHelp : LabelDefs -> Token -> List Token -> Result Error Inst
parseInstHelp labelDefs fst rest =
    case fst of
        Token (OpCode op) _ ->
            case op of
                NOP ->
                    withoutOperand Nop rest

                JMP ->
                    with1Operand (parseJumpInst labelDefs Jmp) fst rest

                MOV ->
                    with2Operands parseMovInst fst rest

        _ ->
            invalidOpCode fst


parseJumpInst : LabelDefs -> (String -> value) -> Token -> Result Error value
parseJumpInst labelDefs fn (Token _ (Loc col string)) =
    if Dict.member string labelDefs then
        Ok (fn string)

    else
        Err (UndefinedLabel col string)


parseMovInst : Token -> Token -> Result Error Inst
parseMovInst a b =
    Result.map2 Mov (parseSrcOperand a) (parseDstOperand b)


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


withoutOperand : v -> List Token -> Result Error v
withoutOperand v rest =
    case rest of
        [] ->
            Ok v

        x :: xs ->
            tooManyOperands x xs


with1Operand :
    (Token -> Result Error value)
    -> Token
    -> List Token
    -> Result Error value
with1Operand fn fst rest =
    case rest of
        [] ->
            missingOperand fst fst

        a :: [] ->
            fn a

        _ :: x :: xs ->
            tooManyOperands x xs


with2Operands :
    (Token -> Token -> Result Error value)
    -> Token
    -> List Token
    -> Result Error value
with2Operands fn fst rest =
    case rest of
        [] ->
            missingOperand fst fst

        a :: [] ->
            missingOperand fst a

        a :: b :: [] ->
            fn a b

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


invalidOpCode : Token -> Result Error value
invalidOpCode (Token _ (Loc col string)) =
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


lexLine : String -> Result Error (List Token)
lexLine srcLine =
    Parser.run tokenListParser (String.toLower srcLine)
        |> Result.mapError (always InternalError)


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
    succeed identity
        |. spaces
        |= oneOf
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



--|. commit ()


labelVariable : Parser String
labelVariable =
    variable
        { start = Char.isAlphaNum
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
