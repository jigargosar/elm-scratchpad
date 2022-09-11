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
                |> expectErr (InvalidOp "lab")
    , test "invalid op or label char" <|
        \_ ->
            Compiler.rawCompile "_ "
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
    , test "invalid op after label" <|
        \_ ->
            Compiler.compile "lab: flop"
                |> expectErr (InvalidOp "flop")
    , test "too many args" <|
        \_ ->
            Compiler.rawCompile "nop left"
                |> Expect.equal
                    (Err
                        [ { row = 1
                          , col = 5
                          , contextStack =
                                [ { col = 4, context = CAfterInst, row = 1 } ]
                          , problem = ExpectingStmtEnd
                          }
                        ]
                    )
    ]


expectErr : Error -> Result Error value -> Expectation
expectErr expected result =
    case result of
        Err actual ->
            actual
                |> Expect.equal expected

        _ ->
            Debug.toString result
                |> Expect.equal ("Compiler Problem: " ++ Debug.toString expected)



--noinspection ElmUnusedSymbol


expectProblem :
    Problem
    -> Result DeadEnds value
    -> Expectation
expectProblem expected result =
    case result of
        Err ({ problem } :: []) ->
            expected
                |> Expect.equal problem

        _ ->
            Debug.toString result
                |> Expect.equal ("Compiler Problem: " ++ Debug.toString expected)
