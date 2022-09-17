module TIS100.PuzzlePage.CompilerV2 exposing
    ( Error(..)
    , ErrorDetail
    , compile
    , compileLine
    , errorsToRecord
    , lexLine
    , prefixLabelToken
    , wordToken
    )

import List.Extra
import Parser exposing (..)
import Set
import Utils as U


type Error
    = InvalidOpCode Int String
    | MissingOperand Int Int
    | TooManyOperands Int Int
    | InternalError


type alias ErrorDetail =
    { row : Int
    , startCol : Int
    , endCol : Int
    , msg : String
    }


type alias Errors =
    List ( Int, Error )


errorsToRecord : Errors -> List ErrorDetail
errorsToRecord =
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


compile : String -> Result Errors value
compile string =
    string
        |> String.split "\n"
        |> List.indexedMap U.pair
        |> List.map (U.biMap U.inc compileLine)
        |> List.foldr
            (\( row, result ) errAcc ->
                case result of
                    Err err ->
                        ( row, err ) :: errAcc

                    Ok _ ->
                        errAcc
            )
            []
        |> Err


compileLine : String -> Result Error Stmt
compileLine src =
    case lexLine src of
        Ok tokens ->
            parseLine tokens

        Err _ ->
            Err InternalError


type Stmt
    = Stmt (Maybe String) (Maybe Inst)


parseLine : List Token -> Result Error Stmt
parseLine tokens =
    case tokens of
        (Token (PrefixLabel lbl) _) :: rest ->
            parseInst rest
                |> Result.map (Stmt (Just lbl))

        _ ->
            parseInst tokens
                |> Result.map (Stmt Nothing)


type Inst
    = Inst


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
        Token (OpCode NOP) _ ->
            withZeroArgOp (\_ -> Inst) rest

        Token (OpCode MOV) _ ->
            with2ArgOp (\_ _ -> Ok Inst) fst rest

        _ ->
            invalidOp fst


withZeroArgOp : (() -> v) -> List Token -> Result Error v
withZeroArgOp fn rest =
    case rest of
        [] ->
            Ok (fn ())

        x :: xs ->
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


invalidOp : Token -> Result Error value
invalidOp l =
    case l of
        Token _ (Loc col string) ->
            Err (InvalidOpCode col string)


lexLine : String -> Result (List DeadEnd) (List Token)
lexLine src =
    Parser.run tokenListParser (String.toLower src)


type Token
    = Token TokenTyp Loc


type Loc
    = Loc Int String


type TokenTyp
    = Word
    | PrefixLabel String
    | OpCode OpCode


type OpCode
    = NOP
    | MOV


wordToken : Int -> String -> Token
wordToken col string =
    Token Word <| Loc col string


prefixLabelToken : Int -> String -> Token
prefixLabelToken col str =
    Token (PrefixLabel str) (Loc col str)


tokenListParser : Parser (List Token)
tokenListParser =
    loop [] tokenListParserHelp


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
        [ backtrackable prefixLabelTokenParser
        , toTokenParser
            (succeed OpCode
                |= oneOf
                    [ opCodeParser MOV "mov"
                    , opCodeParser NOP "nop"
                    ]
            )
        , toTokenParser
            (succeed Word
                |. variable
                    { start = isWordChar
                    , inner = isWordChar
                    , reserved = Set.empty
                    }
            )
        ]


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


opCodeParser : OpCode -> String -> Parser OpCode
opCodeParser opCode string =
    succeed opCode |. keyword string


isWordChar : Char -> Bool
isWordChar c =
    c /= ' ' && c /= '#'
