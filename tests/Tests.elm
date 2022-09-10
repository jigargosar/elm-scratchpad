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
                    |> Expect.equal
                        (Err
                            [ { row = 1
                              , col = 4
                              , contextStack =
                                    [ { row = 1
                                      , col = 1
                                      , context = CLabelPrefix
                                      }
                                    ]
                              , problem = ExpectingLabelSep
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
