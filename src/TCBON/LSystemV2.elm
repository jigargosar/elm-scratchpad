module TCBON.LSystemV2 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Svg exposing (Svg)
import Utils exposing (..)


main =
    div []
        [ lsys { depth = 5, rules = rulesDict, axiom = axiom }
        , div []
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


render : List C2 -> Html msg
render str =
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
            (drawC2List
                { p = vZero
                , a = degrees -90
                , da = degrees 20
                , len = 60
                , prev = None
                }
                str
                []
            )
        ]


drawC2List : Turtle -> List C2 -> List (Svg msg) -> List (Svg msg)
drawC2List t chs acc =
    case chs of
        [] ->
            acc

        (C2 depth h) :: tail ->
            let
                ( nt, res ) =
                    case h of
                        'F' ->
                            let
                                np =
                                    vAdd t.p (vFromPolar ( t.len * (0.65 ^ toFloat depth), t.a ))
                            in
                            ( { t | p = np }, [ vPolyline [ t.p, np ] [] ] )

                        '|' ->
                            let
                                np =
                                    vAdd t.p (vFromPolar ( t.len * (0.65 ^ toFloat depth), t.a ))
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
            drawC2List nt tail (acc ++ res)


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


type alias Config =
    { axiom : String
    , rules : Dict Char String
    , depth : Int
    }


type C2
    = C2 Int Char


parseAxiom : String -> List C2
parseAxiom =
    parseRule 0


parseRule : Int -> String -> List C2
parseRule depth =
    String.toList >> List.map (C2 depth)


expand : Int -> Dict Char String -> List C2 -> List C2
expand depth rules =
    let
        rewriteC2 ((C2 _ ch) as c2) =
            Dict.get ch rules
                |> Maybe.map (parseRule depth)
                |> Maybe.withDefault [ c2 ]
    in
    List.concatMap rewriteC2


lsys : Config -> Html msg
lsys config =
    List.range 1 (config.depth - 1)
        |> List.foldl (\depth -> expand depth config.rules) (parseAxiom config.axiom)
        |> render
