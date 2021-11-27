module TCBON.LSystemV2 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Svg exposing (Svg)
import Utils exposing (..)


main =
    let
        twig : Config
        twig =
            { axiom = "F"
            , rules = [ ( 'F', "|[-F][+F]" ) ]
            , angle = degrees 20
            , length = 100
            , stepSizeFactor = 0.5
            }
    in
    div []
        [ lsys twig 1
        , lsys twig 2
        , lsys twig 7
        ]


render : Config -> List C2 -> Html msg
render config str =
    let
        drawing =
            drawC2List
                { p = vZero
                , a = degrees -90
                , da = degrees 20
                , ds = 0.5
                , len = 150
                , prev = None
                }
                str
                []
    in
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
            drawing
        ]


drawC2List : Turtle -> List C2 -> List (Svg msg) -> List (Svg msg)
drawC2List t chs acc =
    case chs of
        [] ->
            acc

        (C2 depth h) :: tail ->
            let
                factor =
                    t.ds

                ( nt, res ) =
                    case h of
                        'F' ->
                            let
                                np =
                                    vAdd t.p (vFromPolar ( t.len * (factor ^ toFloat depth), t.a ))
                            in
                            ( { t | p = np }, [ vPolyline [ t.p, np ] [] ] )

                        '|' ->
                            let
                                np =
                                    vAdd t.p (vFromPolar ( t.len * (factor ^ toFloat depth), t.a ))
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


renderChar factor depth ch pen =
    case ch of
        'F' ->
            let
                np =
                    vAdd pen.p (vFromPolar ( pen.len * (factor ^ toFloat depth), pen.a ))
            in
            ( { pen | p = np }, [ vPolyline [ pen.p, np ] [] ] )

        '|' ->
            let
                np =
                    vAdd pen.p (vFromPolar ( pen.len * (factor ^ toFloat depth), pen.a ))
            in
            ( { pen | p = np }, [ vPolyline [ pen.p, np ] [] ] )

        '-' ->
            ( { pen | a = pen.a - pen.da }, [] )

        '+' ->
            ( { pen | a = pen.a + pen.da }, [] )

        '[' ->
            ( { pen | prev = Prev pen }, [] )

        ']' ->
            ( case pen.prev of
                None ->
                    pen

                Prev pt ->
                    pt
            , []
            )

        _ ->
            ( pen, [] )


type alias Turtle =
    { p : Vec
    , a : Float
    , da : Float
    , ds : Float
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


type alias Axiom =
    String


type alias Rules =
    List ( Char, String )


type alias Config =
    { axiom : Axiom
    , rules : Rules
    , angle : Float
    , length : Float
    , stepSizeFactor : Float
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


lsys : Config -> Int -> Html msg
lsys config maxDepth =
    let
        rd =
            Dict.fromList config.rules
    in
    List.range 1 (maxDepth - 1)
        |> List.foldl (\depth -> expand depth rd) (parseAxiom config.axiom)
        |> render config
