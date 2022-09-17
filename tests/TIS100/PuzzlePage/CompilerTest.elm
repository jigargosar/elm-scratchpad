module TIS100.PuzzlePage.CompilerTest exposing (..)

import Expect
import TIS100.PuzzlePage.Compiler
    exposing
        ( Error(..)
        , compile
        , compileLine
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


testInvalidSrcCode : Test
testInvalidSrcCode =
    describe "invalid src"
        [ test "invalid ops" <|
            \_ ->
                " foo \n bar \n:"
                    |> compile
                    |> Expect.equal
                        (Err
                            [ ( 1, InvalidOpCode 2 "foo" )
                            , ( 2, InvalidOpCode 2 "bar" )
                            , ( 3, InvalidOpCode 1 ":" )
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
                    |> Expect.equal (Err <| InvalidOpCode 2 "foo")
        , test "invalid op after label" <|
            \_ ->
                "label: foo "
                    |> compileLine
                    |> Expect.equal (Err <| InvalidOpCode 8 "foo")
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
