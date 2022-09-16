module TIS100.PuzzlePage.CompilerV2 exposing
    ( Error(..)
    , compile
    , compileLine
    , labelToken
    , lex
    , wordToken
    )

import Parser exposing (..)
import Set
import Utils as U


type Error
    = InvalidOp Int String
    | InternalError


type alias ErrInfo =
    { row : Int
    , colStart : Int
    , colEnd : Int
    , msg : String
    }


type alias Errors =
    List ( Int, Error )


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
            Err (InvalidOp col string)

        LabelSep col ->
            Err (InvalidOp col ":")


lex : String -> Result (List DeadEnd) (List Token)
lex src =
    Parser.run tokenListParser src


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
