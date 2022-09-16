module Compiler2Test exposing (..)

import Expect
import TIS100.PuzzlePage.CompilerV2
    exposing
        ( ErrTyp(..)
        , Error
        , compile
        , labelToken
        , lex
        , wordToken
        )
import Test exposing (Test, describe, test)



{-
   npx elm-test-rs --watch tests/Compiler2Test.elm
-}


testValidStmts : Test
testValidStmts =
    describe "valid stmts"
        ([ "nop"
         , "lab : nop"
         , "nop : nop"
         ]
            |> List.map
                (\s ->
                    test s (\_ -> Expect.ok (compile s))
                )
        )


testInvalidStmts : Test
testInvalidStmts =
    describe "invalid stmts"
        [ test "invalid op" <|
            \_ ->
                " foo "
                    |> compile
                    |> Expect.equal (Err <| Error 2 (InvalidOp "foo"))
        , test "invalid op after label" <|
            \_ ->
                "label: foo "
                    |> compile
                    |> Expect.equal (Err <| Error 8 (InvalidOp "foo"))
        ]


testLexer : Test
testLexer =
    describe "lexer"
        [ test "single word" <|
            \_ ->
                " foo "
                    |> lex
                    |> Expect.equal (Ok [ wordToken 2 "foo" ])
        , test "single word with symbols" <|
            \_ ->
                " !@# "
                    |> lex
                    |> Expect.equal (Ok [ wordToken 2 "!@#" ])
        , test "two word" <|
            \_ ->
                " foo bar"
                    |> lex
                    |> Expect.equal
                        (Ok
                            [ wordToken 2 "foo"
                            , wordToken 6 "bar"
                            ]
                        )
        , test "comment" <|
            \_ ->
                " foo # bar"
                    |> lex
                    |> Expect.equal (Ok [ wordToken 2 "foo" ])
        , test "no token" <|
            \_ ->
                "  "
                    |> lex
                    |> Expect.equal (Ok [])
        , test "label sep" <|
            \_ ->
                " : "
                    |> lex
                    |> Expect.equal (Ok [ labelToken 2 ])
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
