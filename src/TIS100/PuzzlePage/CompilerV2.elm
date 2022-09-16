module TIS100.PuzzlePage.CompilerV2 exposing
    ( Error(..)
    , ErrorDetail
    , compile
    , compileLine
    , errorsToRecord
    , labelToken
    , lex
    , wordToken
    )

import List.Extra
import Parser exposing (..)
import Set
import Utils as U


type Error
    = InvalidOpCode Int String
    | MissingOperand Int
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

                MissingOperand endCol ->
                    Just
                        { row = l
                        , startCol = 1
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
        _ :: (LabelSep _) :: rest ->
            parseInst rest

        _ ->
            parseInst tokens


parseInst : List Token -> Result Error ()
parseInst tokens =
    case tokens of
        [] ->
            Ok ()

        (Word _ "nop") :: [] ->
            Ok ()

        ((Word _ "mov") as fst) :: rest ->
            case rest of
                [] ->
                    missingOperand fst

                last :: [] ->
                    missingOperand last

                a :: b :: [] ->
                    --parseMoveInst a b
                    Ok ()

                _ :: _ :: x :: xs ->
                    tooManyOperands x xs

        f :: _ ->
            invalidOp f


tooManyOperands : Token -> List Token -> Result Error value
tooManyOperands x xs =
    let
        last =
            List.Extra.last xs
                |> Maybe.withDefault x

        startCol =
            tokenStartColumn x

        endCol =
            tokenEndColumn last
    in
    Err (TooManyOperands startCol endCol)


missingOperand : Token -> Result Error value
missingOperand token =
    Err (MissingOperand (tokenEndColumn token))


tokenStartColumn : Token -> Int
tokenStartColumn token =
    case token of
        Word col _ ->
            col

        LabelSep col ->
            col


tokenEndColumn : Token -> Int
tokenEndColumn token =
    case token of
        Word col string ->
            col + String.length string - 1

        LabelSep col ->
            col


parseZeroArgInst : Token -> Result Error ()
parseZeroArgInst l =
    case l of
        Word _ "nop" ->
            Ok ()

        _ ->
            invalidOp l


invalidOp : Token -> Result Error value
invalidOp l =
    case l of
        Word col string ->
            Err (InvalidOpCode col string)

        LabelSep col ->
            Err (InvalidOpCode col ":")


lex : String -> Result (List DeadEnd) (List Token)
lex src =
    Parser.run tokenListParser (String.toLower src)


type Token
    = Word Int String
    | LabelSep Int


wordToken : Int -> String -> Token
wordToken col string =
    Word col string


labelToken : Int -> Token
labelToken col =
    LabelSep col


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
        [ wordParser
        , succeed LabelSep
            |= getCol
            |. symbol ":"
        ]


wordParser =
    succeed Word
        |= getCol
        |= variable
            { start = \c -> c /= ' ' && c /= ':'
            , inner = \c -> c /= ' ' && c /= ':'
            , reserved = Set.empty
            }
