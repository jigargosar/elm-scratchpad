module Tests exposing (..)

import Expect exposing (Expectation)
import Parser.Advanced exposing (DeadEnd)
import TIS100.PuzzlePage.Compiler as Compiler exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Compiler"
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
        , test "invalid op" <|
            \_ ->
                Compiler.compile "abc "
                    |> expectErr (InvalidOp "abc")
        , test "invalid op after label" <|
            \_ ->
                Compiler.compile "abc: xyz"
                    |> expectErr (InvalidOp "xyz")
        , test "raw: invalid op after label" <|
            \_ ->
                Compiler.rawCompile "abc: xyz"
                    |> Expect.equal
                        (Err
                            [ { col = 9
                              , contextStack =
                                    [ { col = 9, context = COp "xyz", row = 1 }
                                    ]
                              , problem = ExpectingOp
                              , row = 1
                              }
                            ]
                        )
        , test "label cannot be reserved keyword" <|
            \_ ->
                Compiler.rawCompile "nop :"
                    |> Expect.equal
                        (Err
                            [ { row = 1
                              , col = 5
                              , contextStack =
                                    [ { row = 1
                                      , col = 1
                                      , context = CInst
                                      }
                                    ]
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
