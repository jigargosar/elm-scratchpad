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
                        [ { col = 5
                          , contextStack =
                                [ { col = 4, context = CLabelDef "lab", row = 1 } ]
                          , problem = ExpectingLabelSep
                          , row = 1
                          }
                        ]
                    )
    , test "invalid op or label char" <|
        \_ ->
            Compiler.compileRaw "_ "
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 1
                          , contextStack = []
                          , problem = ExpectingLabelVar
                          }
                        , { row = 1
                          , col = 1
                          , contextStack = []
                          , problem = ExpectingOp
                          }
                        ]
                    )
    , test "unknown op after label" <|
        \_ ->
            Compiler.compile "lab: flop"
                |> Expect.equal
                    (Err
                        [ { col = 10
                          , contextStack =
                                [ { col = 10, context = COp "flop", row = 1 }
                                ]
                          , problem = ExpectingOp
                          , row = 1
                          }
                        ]
                    )
    , test "illegal op var after label" <|
        \_ ->
            Compiler.compileRaw "lab: 1"
                |> Expect.equal
                    (Err
                        [ { col = 6
                          , contextStack = []
                          , problem = ExpectingOp
                          , row = 1
                          }
                        , { col = 6
                          , contextStack = []
                          , problem = ExpectingStmtEnd
                          , row = 1
                          }
                        ]
                    )
    , test "too many args" <|
        \_ ->
            Compiler.compileRaw "nop left"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 5
                          , contextStack = []
                          , problem = ExpectingStmtEnd
                          }
                        ]
                    )
    ]
