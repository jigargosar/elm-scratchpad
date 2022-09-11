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
        , describe "should allow" reservedKeywordAsLabel
        ]


reservedKeywordAsLabel =
    [ test "nop:" <|
        \_ ->
            Compiler.compile "nop:"
                |> Expect.equal (Ok (OnlyLabel "nop"))
    , test "nop:nop " <|
        \_ ->
            Compiler.compile "nop:nop"
                |> Expect.equal (Ok (LabelInst "nop" INop))
    ]


validStatement : List Test
validStatement =
    [ test "labeled inst" <|
        \_ ->
            Compiler.compile "a : nop"
                |> Expect.equal (Ok (LabelInst "a" INop))
    , test "only label" <|
        \_ ->
            Compiler.compile "a : "
                |> Expect.equal (Ok (OnlyLabel "a"))
    , test "only inst" <|
        \_ ->
            Compiler.compile "nop "
                |> Expect.equal (Ok (OnlyInst INop))
    , test "comment after label" <|
        \_ ->
            Compiler.compile "lab:#comment "
                |> Expect.equal (Ok (OnlyLabel "lab"))
    ]


invalidStatement : List Test
invalidStatement =
    [ test "invalid op" <|
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
    , test "unknown op after label" <|
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
    , test "illegal op var after label" <|
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
