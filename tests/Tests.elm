module Tests exposing (..)

import Expect
import TIS100.PuzzlePage.Compiler as Compiler exposing (Context(..), Inst(..), Problem(..), Stmt(..))
import Test exposing (..)


suite : Test
suite =
    describe "Compiler"
        [ {-
             "Implement the first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
          -}
          test "labeled inst" <|
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
                Compiler.compile "abc"
                    |> expectProblem ExpectingLabelSep
        , test "invalid op after label" <|
            \_ ->
                Compiler.compile "abc: xyz"
                    |> Expect.equal
                        (Err
                            [ { row = 1
                              , col = 6
                              , contextStack = [ { col = 6, context = CInst, row = 1 } ]
                              , problem = ExpectingOp
                              }
                            , { col = 6
                              , contextStack = [ { col = 6, context = CInst, row = 1 } ]
                              , problem = ExpectingOp
                              , row = 1
                              }
                            , { row = 1
                              , col = 6
                              , contextStack = []
                              , problem = ExpectingStmtEnd
                              }
                            ]
                        )
        , test "label cannot be reserved keyword" <|
            \_ ->
                Compiler.compile "nop :"
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


expectProblem : a -> Result (List { b | problem : a }) value -> Expect.Expectation
expectProblem expecting result =
    case result of
        Err ({ problem } :: []) ->
            expecting
                |> Expect.equal problem

        _ ->
            Expect.fail "expectProblem failed"
