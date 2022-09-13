module Compiler2Test exposing (..)

import Expect
import List.Extra
import Test exposing (Test, describe, test)
import Utils as U



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
    let
        col =
            s
                |> String.toList
                |> List.indexedMap Tuple.pair
                |> List.Extra.dropWhile (U.second >> U.eq ' ')
                |> List.head
                |> Maybe.map U.first
                |> Maybe.withDefault 0
                |> Debug.log "Debug: "
    in
    Err (Error InvalidOp s col)


suite : Test
suite =
    test "invalid op" <|
        \_ ->
            expectErrorOnCompile
                ( "  foo"
                , "--^"
                , InvalidOp
                )


expectErrorOnCompile : ( String, String, Prob ) -> Expect.Expectation
expectErrorOnCompile ( src, marker, prob ) =
    case compile src of
        Err err ->
            ( src, String.repeat err.col "-" ++ "^", err.msg )
                |> Expect.equal ( src, marker, prob )

        Ok _ ->
            Debug.todo "todo"
