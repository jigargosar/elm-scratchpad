module TCBON.LSystem exposing (..)

import Dict
import Html exposing (text)


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
    [ "B"
    , "F[-B]+B"
    ]
        |> always (applyRules axiom)
