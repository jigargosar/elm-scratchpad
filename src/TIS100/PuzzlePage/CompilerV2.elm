module TIS100.PuzzlePage.CompilerV2 exposing
    ( Error(..)
    , ErrorDetail
    , compile
    , compileLine
    , errorsToRecord
    , lex
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


compile : String -> Result Errors ()
compile string =
    string
        |> String.split "\n"
        |> List.indexedMap U.pair
        |> List.map (U.biMap U.inc compileLine)
        |> List.foldr
            (\( li, res ) errAcc ->
                case res of
                    Err err ->
                        ( li, err ) :: errAcc

                    Ok _ ->
                        errAcc
            )
            []
        |> Err


compileLine : String -> Result Error ()
compileLine src =
    case lex src of
        Ok value ->
            parseLine value

        Err _ ->
            Err InternalError


parseLine : List Token -> Result Error ()
parseLine tokens =
    case tokens of
        (Token PrefixLabel _ _) :: rest ->
            parseInst rest

        _ ->
            parseInst tokens


parseInst : List Token -> Result Error ()
parseInst tokens =
    case tokens of
        [] ->
            Ok ()

        (Token (OpCode NOP) _ _) :: [] ->
            Ok ()

        ((Token (OpCode MOV) _ _) as fst) :: rest ->
            case rest of
                [] ->
                    missingOperand fst fst

                last :: [] ->
                    missingOperand fst last

                _ :: _ :: [] ->
                    --parseMoveInst a b
                    Ok ()

                _ :: _ :: x :: xs ->
                    tooManyOperands x xs

        f :: _ ->
            invalidOp f


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
        Token _ col _ ->
            col


tokenEndColumn : Token -> Int
tokenEndColumn token =
    case token of
        Token _ col string ->
            col + String.length string - 1


invalidOp : Token -> Result Error value
invalidOp l =
    case l of
        Token _ col string ->
            Err (InvalidOpCode col string)


lex : String -> Result (List DeadEnd) (List Token)
lex src =
    Parser.run tokenListParser (String.toLower src)


type Token
    = Token TokenTyp Int String


type TokenTyp
    = Word
    | PrefixLabel
    | OpCode OpCode


type OpCode
    = NOP
    | MOV


wordToken : Int -> String -> Token
wordToken col string =
    Token Word col string


prefixLabelToken : Int -> String -> Token
prefixLabelToken col str =
    Token PrefixLabel col str


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
        [ backtrackable
            (succeed (Token PrefixLabel)
                |= getCol
                |= variable
                    { start = Char.isAlpha
                    , inner = Char.isAlphaNum
                    , reserved = Set.empty
                    }
                |. spaces
                |. symbol ":"
            )
        , succeed (\col ( opCode, string ) -> Token (OpCode opCode) col string)
            |= getCol
            |= oneOf
                [ opCodeParser MOV "mov"
                , opCodeParser NOP "nop"
                ]
        , succeed (Token Word)
            |= getCol
            |= variable
                { start = isWordChar
                , inner = isWordChar
                , reserved = Set.empty
                }
        ]


opCodeParser : OpCode -> String -> Parser ( OpCode, String )
opCodeParser opCode string =
    succeed ( opCode, string ) |. keyword string


isWordChar : Char -> Bool
isWordChar c =
    c /= ' ' && c /= '#'
