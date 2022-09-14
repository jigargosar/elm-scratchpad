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


type Token
    = Word String
    | LabelSep
    | Comment


tokenListParser : Parser (List Token)
tokenListParser =
    loop [] tokenListParserHelp


tokenListParserHelp : List Token -> Parser (Step (List Token) (List Token))
tokenListParserHelp rs =
    oneOf
        [ succeed (\t -> Loop (t :: rs))
            |= tokenParser
        , succeed ()
            --|. end
            |> map (\_ -> Done <| List.reverse rs)
        ]


tokenParser : Parser Token
tokenParser =
    succeed identity
        |. spaces
        |= oneOf
            [ --getChompedString (chompWhile (\c -> c /= ' ' && c /= ':'))
              variable
                { start = \c -> c /= ' ' && c /= ':'
                , inner = \c -> c /= ' ' && c /= ':'
                , reserved = Set.empty
                }
                |> map Word
            , chompIf (\c -> c == ':')
                |> map (\_ -> LabelSep)
            , succeed Comment |. lineComment "#"
            ]
        |. spaces


testLexer : Test
testLexer =
    test "tokens" <|
        \_ ->
            " foo "
                |> lex
                |> Expect.equal (Ok [ Word "foo" ])



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
