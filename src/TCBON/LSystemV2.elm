module TCBON.LSystemV2 exposing (..)

import Dict
import Html exposing (div, text)
import Svg
import Utils exposing (..)


main =
    div []
        [ div []
            (results
                |> List.reverse
                |> List.take 2
                |> List.map drawResult
            )
        , div []
            (results
                |> List.map viewResult
            )
        ]


drawResult str =
    Svg.svg
        [ style "width" "100vw"
        , style "height" "40vh"
        , dBlock
        , noFill
        , noStroke
        ]
        [ group
            [ style "transform" "translate(0%,50%)"
            , strokeW 1
            , stroke black
            ]
            (drawStr (Turtle vZero 0 (degrees 20) 1 None) (String.toList str) [])
        ]


type alias Turtle =
    { p : Vec
    , a : Float
    , ad : Float
    , len : Float
    , prev : Prev
    }


type Prev
    = None
    | Prev Turtle


drawStr t chs acc =
    case chs of
        [] ->
            acc

        h :: tail ->
            let
                ( nt, res ) =
                    case h of
                        'F' ->
                            let
                                np =
                                    vAdd t.p (vFromPolar ( t.len, t.a ))
                            in
                            ( { t | p = np }, [ vPolyline [ t.p, np ] [] ] )

                        '-' ->
                            ( { t | a = t.a - t.ad }, [] )

                        '+' ->
                            ( { t | a = t.a + t.ad }, [] )

                        '[' ->
                            ( { t | prev = Prev t }, [] )

                        ']' ->
                            ( case t.prev of
                                None ->
                                    t

                                Prev pt ->
                                    pt
                            , []
                            )

                        _ ->
                            ( t, [] )
            in
            drawStr nt tail (acc ++ res)


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
    scanApplyN 9 applyRules axiom
