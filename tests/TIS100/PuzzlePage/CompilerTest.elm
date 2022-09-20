module TIS100.PuzzlePage.CompilerTest exposing (..)

import Expect
import TIS100.PuzzlePage.Compiler
    exposing
        ( Error(..)
        , compile
        , lexLine
        , prefixLabelToken
        , wordToken
        )
import Test exposing (Test, describe, test)



{-
   npx elm-test-rs --watch tests/**/CompilerTest.elm
-}


testValidStmts : Test
testValidStmts =
    test "valid src" <|
        \_ ->
            [ "nop"
            , "lab : nop"
            , "nop : nop"
            , " nop"
            , "jmp lab"
            ]
                |> compileLines
                |> Expect.ok


compileLines ls =
    ls
        |> String.join "\n"
        |> compile


testInvalidSrcCode : Test
testInvalidSrcCode =
    describe "invalid src code"
        [ test "invalid ops" <|
            \_ ->
                [ " foo"
                , ":"
                , " label: foo"
                , "jmp undef"
                , " a:"
                , " a:"
                ]
                    |> compileLines
                    |> Expect.equal
                        (Err
                            [ ( 1, InvalidOpCode 2 "foo" )
                            , ( 2, InvalidOpCode 1 ":" )
                            , ( 3, InvalidOpCode 9 "foo" )
                            , ( 4, UndefinedLabel 5 "undef" )
                            , ( 6, LabelAlreadyDefined 2 "a" )
                            ]
                        )
        ]


testLexer : Test
testLexer =
    describe "lexer"
        [ test "single word" <|
            \_ ->
                " foo "
                    |> lexLine
                    |> Expect.equal (Ok [ wordToken 2 "foo" ])
        , test "single word with symbols" <|
            \_ ->
                " !@$ "
                    |> lexLine
                    |> Expect.equal (Ok [ wordToken 2 "!@$" ])
        , test "two word" <|
            \_ ->
                " foo bar"
                    |> lexLine
                    |> Expect.equal
                        (Ok
                            [ wordToken 2 "foo"
                            , wordToken 6 "bar"
                            ]
                        )
        , test "comment" <|
            \_ ->
                " foo# bar"
                    |> lexLine
                    |> Expect.equal (Ok [ wordToken 2 "foo" ])
        , test "no token" <|
            \_ ->
                "  "
                    |> lexLine
                    |> Expect.equal (Ok [])
        , test "label sep" <|
            \_ ->
                " : "
                    |> lexLine
                    |> Expect.equal (Ok [ wordToken 2 ":" ])
        , test "label token" <|
            \_ ->
                " label : "
                    |> lexLine
                    |> Expect.equal (Ok [ prefixLabelToken 2 "label" ])
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
