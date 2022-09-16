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

import Parser exposing (..)
import Set
import Utils as U


type Error
    = InvalidOpCode Int String
    | MissingOperand
    | TooManyOperands
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
                InvalidOpCode col string ->
                    Just
                        { row = l
                        , startCol = col
                        , endCol = col + String.length string
                        , msg = "Invalid op:\"" ++ string ++ "\""
                        }

                MissingOperand ->
                    Just
                        { row = l
                        , startCol = 0
                        , endCol = 1
                        , msg = "missing operand"
                        }

                TooManyOperands ->
                    Just
                        { row = l
                        , startCol = 0
                        , endCol = 1
                        , msg = "missing operand"
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

        f :: [] ->
            parseZeroArgInst f

        (Word _ "mov") :: rest ->
            case rest of
                _ :: _ :: _ :: _ ->
                    Err TooManyOperands

                a :: b :: [] ->
                    --parseMoveInst a b
                    Ok ()

                _ ->
                    Err MissingOperand

        f :: _ ->
            invalidOp f


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
