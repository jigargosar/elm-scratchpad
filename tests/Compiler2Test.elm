module Compiler2Test exposing (..)

import Expect
import Test exposing (Test, describe, test)



{-
   npx elm-test-rs --watch tests/Compiler2Test.elm
-}


type Error
    = InvalidOp String


compile : String -> Result Error ()
compile s =
    Err (InvalidOp s)


suite : Test
suite =
    test "invalid op" <|
        \_ ->
            compile "foo"
                |> Expect.equal (Err (InvalidOp "foo"))
