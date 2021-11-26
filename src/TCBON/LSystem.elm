module TCBON.LSystem exposing (..)

import Dict
import Html exposing (div, text)
import Utils exposing (..)


main =
    div [] (List.map viewResult results)


viewResult r =
    div [] [ text r ]


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
    scanApplyN 3 applyRules axiom
