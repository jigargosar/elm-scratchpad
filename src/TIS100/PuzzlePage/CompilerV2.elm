module TIS100.PuzzlePage.CompilerV2 exposing (ErrTyp(..), Error, Located, Token(..), compile, labelToken, lex, wordToken)

import Parser exposing (..)
import Set


type ErrTyp
    = InvalidOp String
    | InternalError


type alias Error =
    { col : Int
    , typ : ErrTyp
    }


compile : String -> Result Error ()
compile src =
    case lex src of
        Ok value ->
            parse value

        Err _ ->
            Err (Error 0 InternalError)


parse : List Located -> Result Error ()
parse tokens =
    case tokens of
        _ :: s :: rest ->
            if s.token == LabelSep then
                parseInst rest

            else
                parseInst tokens

        _ ->
            parseInst tokens


parseInst : List Located -> Result Error ()
parseInst tokens =
    case tokens of
        [] ->
            Ok ()

        f :: [] ->
            parseZeroArgInst f

        f :: _ ->
            invalidOp f


parseZeroArgInst : Located -> Result Error ()
parseZeroArgInst l =
    case l.token of
        Word "nop" ->
            Ok ()

        _ ->
            invalidOp l


invalidOp : Located -> Result Error value
invalidOp l =
    Err (Error l.col (InvalidOp (tokenToString l.token)))


lex : String -> Result (List DeadEnd) (List Located)
lex src =
    Parser.run tokenListParser src


type alias Located =
    { col : Int
    , token : Token
    }


type Token
    = Word String
    | LabelSep


wordToken : Int -> String -> Located
wordToken col string =
    Located col (Word string)


labelToken : Int -> Located
labelToken col =
    Located col LabelSep


tokenToString : Token -> String
tokenToString token =
    case token of
        Word string ->
            string

        LabelSep ->
            ":"


tokenListParser : Parser (List Located)
tokenListParser =
    loop [] tokenListParserHelp


tokenListParserHelp : List Located -> Parser (Step (List Located) (List Located))
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


tokenParser : Parser Located
tokenParser =
    succeed Located
        |= getCol
        |= oneOf
            [ succeed Word
                |= wordParser
            , succeed LabelSep
                |. symbol ":"
            ]


wordParser : Parser String
wordParser =
    variable
        { start = \c -> c /= ' ' && c /= ':'
        , inner = \c -> c /= ' ' && c /= ':'
        , reserved = Set.empty
        }
