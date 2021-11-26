module TCBON.LSystemV2 exposing (..)

import Dict
import Html exposing (Html, div, text)
import Svg exposing (Svg)
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


drawResult : String -> Html msg
drawResult str =
    Svg.svg
        [ style "width" "100vw"
        , style "height" "40vh"
        , dBlock
        , noFill
        , noStroke
        , overflowVisible
        ]
        [ group
            [ style "transform" "translate(50%,50%)"
            , strokeW 1
            , stroke black
            ]
            (drawStr
                { p = vZero
                , a = degrees -90
                , da = degrees 20
                , len = 30
                , prev = None
                }
                (String.toList str)
                []
            )
        ]


type alias Turtle =
    { p : Vec
    , a : Float
    , da : Float
    , len : Float
    , prev : Prev
    }


type Prev
    = None
    | Prev Turtle


drawStr : Turtle -> List Char -> List (Svg msg) -> List (Svg msg)
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

                        '|' ->
                            let
                                np =
                                    vAdd t.p (vFromPolar ( t.len, t.a ))
                            in
                            ( { t | p = np }, [ vPolyline [ t.p, np ] [] ] )

                        '-' ->
                            ( { t | a = t.a - t.da }, [] )

                        '+' ->
                            ( { t | a = t.a + t.da }, [] )

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
    "F"


rulesDict =
    Dict.fromList
        [ ( 'F', "|[-F][+F]" )
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
    scanApplyN 5 applyRules axiom
