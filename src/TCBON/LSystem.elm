module TCBON.LSystem exposing (..)

import Dict
import Html exposing (div, text)
import Svg
import Utils exposing (..)


main =
    div []
        [ div [] (List.map viewResult results)
        , div [] (List.map drawResult results)
        ]


drawResult str =
    Svg.svg
        [ style "width" "100vw"
        , style "height" "20vh"
        , dBlock
        , noFill
        , noStroke
        ]
        [ group
            [ style "transform" "translate(50%,50%)"
            , strokeW 1
            , stroke black
            ]
            [ words str []
            ]
        ]


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
