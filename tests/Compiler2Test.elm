module Compiler2Test exposing (..)

import Expect
import List.Extra
import Parser exposing (..)
import Set
import Test exposing (Test, describe, skip, test)
import Utils as U



{-
   npx elm-test-rs --watch tests/Compiler2Test.elm
-}


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
        [] ->
            Ok ()

        only :: _ ->
            Err (Error only.col (InvalidOp (tokenToString only.token)))


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


tokenListParserHelp rs =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed (Loop rs)
                |. lineComment "#"
            , succeed (\t -> Loop (t :: rs))
                |= locatedTokenParser
            , succeed ()
                |. end
                |> map (\_ -> Done <| List.reverse rs)
            ]


locatedTokenParser : Parser Located
locatedTokenParser =
    succeed Located
        |= getCol
        |= tokenParser


tokenParser : Parser Token
tokenParser =
    oneOf
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


testCompilerErrors : Test
testCompilerErrors =
    describe "compiler"
        [ test "invalid op" <|
            \_ ->
                " foo "
                    |> compile
                    |> Expect.equal (Err <| Error 2 (InvalidOp "foo"))
        ]


testLexer : Test
testLexer =
    describe "lexer"
        [ test "single word" <|
            \_ ->
                " foo "
                    |> lex
                    |> Expect.equal (Ok [ Located 2 (Word "foo") ])
        , test "single word with symbols" <|
            \_ ->
                " !@# "
                    |> lex
                    |> Expect.equal (Ok [ Located 2 (Word "!@#") ])
        , test "two word" <|
            \_ ->
                " foo bar"
                    |> lex
                    |> Expect.equal
                        (Ok
                            [ Located 2 <| Word "foo"
                            , Located 6 <| Word "bar"
                            ]
                        )
        , test "comment" <|
            \_ ->
                " foo # bar"
                    |> lex
                    |> Expect.equal (Ok [ Located 2 (Word "foo") ])
        , test "no token" <|
            \_ ->
                "  "
                    |> lex
                    |> Expect.equal (Ok [])
        , test "label sep" <|
            \_ ->
                " : "
                    |> lex
                    |> Expect.equal (Ok [ Located 2 LabelSep ])
        ]



--suite : Test
--suite =
--    describe "errors"
--        [ test "invalid op" <|
--            \_ ->
--                expectErrorOnCompile
--                    ( "  foo"
--                    , "--^"
--                    , InvalidOp "foo"
--                    )
--        ]
--
--
--expectErrorOnCompile : ( String, String, ErrTyp ) -> Expect.Expectation
--expectErrorOnCompile ( src, marker, prob ) =
--    case compile src of
--        Err err ->
--            ( src, String.repeat err.col "-" ++ "^", err.typ )
--                |> Expect.equal ( src, marker, prob )
--
--        Ok _ ->
--            Debug.todo "todo"
