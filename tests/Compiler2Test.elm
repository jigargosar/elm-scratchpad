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
--type ErrTyp
--    = InvalidOp String
--
--
--type alias Error =
--    { typ : ErrTyp
--    , col : Int
--    }
--
--
--compile : String -> Result Error ()
--compile src =
--    --Parser.run tokenise src
--    Debug.todo "todo"
--


lex src =
    Parser.run tokenListParser src


type alias LocatedToken =
    { col : Int
    , token : Token
    }


type Token
    = Word String
    | LabelSep


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


locatedTokenParser : Parser LocatedToken
locatedTokenParser =
    succeed LocatedToken
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


testLexer : Test
testLexer =
    describe "lexer"
        [ test "single word" <|
            \_ ->
                " foo "
                    |> lex
                    |> Expect.equal (Ok [ LocatedToken 2 (Word "foo") ])
        , test "two word" <|
            \_ ->
                " foo bar"
                    |> lex
                    |> Expect.equal
                        (Ok
                            [ LocatedToken 2 <| Word "foo"
                            , LocatedToken 6 <| Word "bar"
                            ]
                        )
        , test "comment" <|
            \_ ->
                " foo # bar"
                    |> lex
                    |> Expect.equal (Ok [ LocatedToken 2 (Word "foo") ])
        , test "no token" <|
            \_ ->
                "  "
                    |> lex
                    |> Expect.equal (Ok [])
        , test "label sep" <|
            \_ ->
                " : "
                    |> lex
                    |> Expect.equal (Ok [ LocatedToken 2 LabelSep ])
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
