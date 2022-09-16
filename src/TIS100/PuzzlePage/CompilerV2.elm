module TIS100.PuzzlePage.CompilerV2 exposing
    ( ErrTyp(..)
    , Error
    , compile
    , labelToken
    , lex
    , wordToken
    )

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


parse : List Token -> Result Error ()
parse tokens =
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
            Err (Error col (InvalidOp string))

        LabelSep col ->
            Err (Error col (InvalidOp ":"))


lex : String -> Result (List DeadEnd) (List Token)
lex src =
    Parser.run tokenListParser src


type alias Located =
    { col : Int
    , token : Token
    }


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
