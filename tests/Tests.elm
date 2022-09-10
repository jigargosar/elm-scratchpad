module Tests exposing (..)

import Expect
import TIS100.PuzzlePage.Compiler as Compiler exposing (Inst(..), Problem(..), Stmt(..))
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
        , test "label cannot be reserved keyword" <|
            \_ ->
                Compiler.compile "nop:"
                    |> Expect.equal
                        (Err
                            [ { col = 4
                              , contextStack = []
                              , problem = ExpectingEnd
                              , row = 1
                              }
                            ]
                        )
        ]
