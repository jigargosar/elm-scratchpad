module Compiler2Test exposing (..)

import Expect
import TIS100.PuzzlePage.CompilerV2 exposing (Error(..), compile, compileLine, labelToken, lex, wordToken)
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
                    test s (\_ -> Expect.ok (compileLine s))
                )
        )


testInvalidSrc : Test
testInvalidSrc =
    describe "invalid src"
        [ test "invalid ops" <|
            \_ ->
                " foo \n bar"
                    |> compile
                    |> Expect.equal
                        (Err
                            [ ( 1, InvalidOp 2 "foo" )
                            , ( 2, InvalidOp 2 "bar" )
                            ]
                        )
        ]


testInvalidStmts : Test
testInvalidStmts =
    describe "invalid stmts"
        [ test "invalid op" <|
            \_ ->
                " foo "
                    |> compileLine
                    |> Expect.equal (Err <| InvalidOp 2 "foo")
        , test "invalid op after label" <|
            \_ ->
                "label: foo "
                    |> compileLine
                    |> Expect.equal (Err <| InvalidOp 8 "foo")
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
