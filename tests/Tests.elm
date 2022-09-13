module Tests exposing (..)

import Expect exposing (Expectation)
import Parser.Advanced exposing (DeadEnd)
import TIS100.PuzzlePage.Compiler as Compiler exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Compiler"
        [ describe "should compile" validStatement
        , describe "mov instructions" movInstructions
        , describe "should fail on" invalidStatement
        ]


movInstructions : List Test
movInstructions =
    [ "mov 1 acc"
        |> shouldCompileTo "mov 1 acc"
    , "mov up acc"
        |> shouldCompileTo "mov up acc"
    ]


validStatement : List Test
validStatement =
    [ describe "labeled inst"
        [ "a : nop " |> shouldCompileTo "a: nop" ]
    , describe "only label"
        [ "a : " |> shouldCompileTo "a:" ]
    , describe "only inst"
        [ "nop " |> shouldCompileTo "nop" ]
    , describe "comment after label"
        [ "lab:#comment " |> shouldCompileTo "lab:" ]
    , describe "comment after labeled inst"
        [ "lab:nop#comment " |> shouldCompileTo "lab: nop" ]
    ]


invalidStatement : List Test
invalidStatement =
    [ test "unknown op name" <|
        \_ ->
            Compiler.compile "lab "
                |> Expect.equal
                    (Err { col = 1, problem = InvalidOp })
    , test "invalid first start char" <|
        \_ ->
            Compiler.compile "_ "
                |> Expect.equal
                    (Err { col = 1, problem = InvalidOp })
    , test "unknown op name after label" <|
        \_ ->
            Compiler.compile "lab: flop"
                |> Expect.equal
                    (Err { col = 6, problem = InvalidOp })
    , test "illegal op char after label" <|
        \_ ->
            Compiler.compile "lab: 1"
                |> Expect.equal
                    (Err { col = 6, problem = InvalidOp })
    , test "too many args" <|
        \_ ->
            expectErrorOnCompile
                ( "nop left"
                , "----^"
                , TooManyArgs
                )
    ]


expectErrorOnCompile : ( String, String, Error ) -> Expect.Expectation
expectErrorOnCompile ( src, marker, prob ) =
    case compile src of
        Err err ->
            ( src, String.repeat (err.col - 1) "-" ++ "^", err.problem )
                |> Expect.equal ( src, marker, prob )

        Ok _ ->
            Debug.todo "todo"


shouldCompileTo : String -> String -> Test
shouldCompileTo expected actual =
    test actual <|
        \_ ->
            Compiler.compile actual
                |> Result.map Compiler.stmtToString
                |> Result.mapError Debug.toString
                |> Expect.equal (Ok expected)
