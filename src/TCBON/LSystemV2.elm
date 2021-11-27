module TCBON.LSystemV2 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg.Attributes as SA
import Utils exposing (..)


main =
    let
        twig : Config
        twig =
            { axiom = "F"
            , rules = [ ( 'F', "|[-F][+F]" ) ]
            , deltaAngle = degrees 20
            , stepSizeFactor = 0.5
            , initialPosition = vZero
            }
    in
    div
        [ dGrid
        , style "width" "100%"
        , style "height" "100%"
        , style "place-content" "stretch"
        , style "grid-auto-flow" "column"
        ]
        [ lsys twig 1, lsys twig 2, lsys twig 7 ]


render : Config -> List C2 -> Html msg
render config chs =
    let
        drawing =
            renderCharList
                { p = config.initialPosition
                , a = degrees -90
                , da = config.deltaAngle
                , ds = config.stepSizeFactor
                , len = 100
                , prev = None
                }
                chs
                []
    in
    Svg.svg
        [ viewBoxC 200 200
        , dBlock
        , noFill
        , noStroke
        , overflowVisible
        , style "outline" "auto blue"
        ]
        [ group
            [ strokeW 1
            , stroke black

            --, style "transform" "translate(0%,0%)"
            ]
            drawing
        ]


renderCharList : Turtle -> List C2 -> List (Svg msg) -> List (Svg msg)
renderCharList t chs acc =
    case chs of
        [] ->
            acc

        (C2 depth h) :: tail ->
            let
                factor =
                    t.ds

                ( nt, res ) =
                    renderChar factor depth h t
            in
            renderCharList nt tail (acc ++ res)


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
    , initialPosition : Vec
    , deltaAngle : Float
    , stepSizeFactor : Float
    }


type C2
    = C2 Int Char


expandAxiom : String -> List C2
expandAxiom =
    expandString 0


expandString : Int -> String -> List C2
expandString depth =
    String.toList >> List.map (C2 depth)


expand : Dict Char String -> Int -> List C2 -> List C2
expand rules depth =
    let
        rewriteC2 ((C2 _ ch) as c2) =
            Dict.get ch rules
                |> Maybe.map (expandString depth)
                |> Maybe.withDefault [ c2 ]
    in
    List.concatMap rewriteC2


lsys : Config -> Int -> Html msg
lsys config maxDepth =
    let
        rulesDict =
            Dict.fromList config.rules
    in
    List.range 1 (maxDepth - 1)
        |> List.foldl (expand rulesDict) (expandAxiom config.axiom)
        |> render config
