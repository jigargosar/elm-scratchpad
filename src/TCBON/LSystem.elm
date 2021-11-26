module TCBON.LSystem exposing (..)

import Dict
import Html exposing (text)
import Utils exposing (..)


main =
    Debug.toString results
        |> text


axiom =
    "B"


rulesDict =
    Dict.fromList
        [ ( 'B', "F[-B]+B" )
        , ( 'F', "FF" )
        ]


applyRules : String -> String
applyRules =
    String.toList
        >> List.foldr
            (\ch ->
                (::) (Dict.get ch rulesDict |> Maybe.withDefault (String.fromChar ch))
            )
            []
        >> String.join ""


results =
    applyN 3 applyRules axiom
