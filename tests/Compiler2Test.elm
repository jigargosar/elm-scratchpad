module Compiler2Test exposing (..)

import Expect
import Test exposing (Test, test)



{-
   npx elm-test-rs --watch tests/Compiler2Test.elm
-}


suite : Test
suite =
    test "foo" <|
        \_ -> Expect.equal True True
