module Tests exposing (..)

import Expect exposing (Expectation)
import Parser.Advanced exposing (DeadEnd)
import TIS100.PuzzlePage.Compiler as Compiler exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Compiler"
        [ describe "should compile" validStatement
        , describe "should fail on" invalidStatement
        , describe "mov instructions" movInstructions
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
                    (Err
                        [ { col = 1
                          , contextStack = []
                          , problem = InvalidOp
                          , row = 1
                          }
                        ]
                    )
    , test "invalid first start char" <|
        \_ ->
            Compiler.compile "_ "
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , contextStack = []
                          , problem = InvalidOp
                          }
                        ]
                    )
    , test "unknown op name after label" <|
        \_ ->
            Compiler.compile "lab: flop"
                |> Expect.equal
                    (Err
                        [ { col = 6
                          , contextStack = []
                          , problem = InvalidOp
                          , row = 1
                          }
                        ]
                    )
    , test "illegal op char after label" <|
        \_ ->
            Compiler.compile "lab: 1"
                |> Expect.equal
                    (Err
                        [ { col = 6
                          , contextStack = []
                          , problem = InvalidOp
                          , row = 1
                          }
                        ]
                    )
    , test "too many args" <|
        \_ ->
            Compiler.compile "nop left"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 5
                          , contextStack = []
                          , problem = TooManyArgs
                          }
                        ]
                    )
    ]


shouldCompileTo : String -> String -> Test
shouldCompileTo expected actual =
    test actual <|
        \_ ->
            Compiler.compile actual
                |> Result.map Compiler.stmtToString
                |> Result.mapError Debug.toString
                |> Expect.equal (Ok expected)
