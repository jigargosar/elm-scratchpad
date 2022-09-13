module Compiler2Test exposing (..)

import Expect
import Test exposing (Test, describe, test)



{-
   npx elm-test-rs --watch tests/Compiler2Test.elm
-}


type Prob
    = InvalidOp


type alias Error =
    { msg : Prob
    , arg : String
    , col : Int
    }


compile : String -> Result Error ()
compile s =
    Err (Error InvalidOp s 0)


suite : Test
suite =
    test "invalid op" <|
        \_ ->
            expectErrorOnCompile
                ( " foo"
                , "-^"
                , InvalidOp
                )


expectErrorOnCompile : ( String, String, Prob ) -> Expect.Expectation
expectErrorOnCompile ( src, marker, prob ) =
    case compile src of
        Err err ->
            ( src, String.repeat (err.col - 1) "-" ++ "^", err.msg )
                |> Expect.equal ( src, marker, prob )

        Ok _ ->
            Debug.todo "todo"
