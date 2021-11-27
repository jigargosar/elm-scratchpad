module TCBON.LSystemV2 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Svg exposing (Svg)
import Utils exposing (..)


main =
    let
        bigH : Config
        bigH =
            { axiom = "[F]--F"
            , rules = [ ( 'F', "|[+F][-F]" ) ]
            , deltaAngle = degrees 90
            , stepSize = 13 / 20
            , origin = vZero
            , initialLength = 40
            , initialAngle = 0
            }

        bentBigH : Config
        bentBigH =
            { axiom = "[F]--F"
            , rules = [ ( 'F', "|[+F][-F]" ) ]
            , deltaAngle = degrees 80
            , stepSize = 13 / 20
            , origin = vZero
            , initialLength = 30
            , initialAngle = degrees -5
            }

        twoYs : Config
        twoYs =
            { axiom = "[F]----F"
            , rules = [ ( 'F', "|[+F][-F]" ) ]
            , deltaAngle = degrees 45
            , stepSize = 13 / 20
            , origin = vZero
            , initialLength = 28
            , initialAngle = 0
            }

        twig : Config
        twig =
            { axiom = "F"
            , rules = [ ( 'F', "|[-F][+F]" ) ]
            , deltaAngle = degrees 20
            , stepSize = 1 / 2
            , origin = vec 0 45
            , initialLength = 90
            , initialAngle = 0
            }

        weed_1 : Config
        weed_1 =
            { axiom = "F"
            , rules = [ ( 'F', "F[-F]F[+F]F" ) ]
            , deltaAngle = degrees 25
            , stepSize = 1 / 2
            , origin = vec 0 45
            , initialLength = 16
            , initialAngle = 0
            }
    in
    div []
        ([ ( bigH, 9 )
         , ( bentBigH, 9 )
         , ( twoYs, 9 )
         , ( twig, 9 )
         , ( weed_1, 5 )
         ]
            |> List.map viewLSys3
        )


viewLSys3 ( c, d ) =
    div [ dGrid, style "grid-auto-flow" "column" ] [ lsys c 1, lsys c 2, lsys c d ]


render : Config -> List C2 -> Html msg
render config chs =
    let
        ( w, h ) =
            ( 100, 100 )

        drawing =
            renderCharList
                { p = config.origin
                , a = degrees -90 + config.initialAngle
                , da = config.deltaAngle
                , ds = config.stepSize
                , len = config.initialLength
                , prev = None
                }
                chs
                []
    in
    Svg.svg
        [ viewBoxC w h
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


type PathCommands
    = LineTo Vec


moveForward : Float -> Int -> Turtle -> ( Turtle, List (Svg msg) )
moveForward factor depth pen =
    let
        np =
            vAdd pen.p (vFromPolar ( pen.len * (factor ^ toFloat depth), pen.a ))
    in
    ( { pen | p = np }
    , [ vPolyline [ pen.p, np ]
            [ style "vector-effect" "non-scaling-stroke"
            ]
      ]
    )


renderChar : Float -> Int -> Char -> Turtle -> ( Turtle, List (Svg msg) )
renderChar factor depth ch pen =
    case ch of
        'F' ->
            moveForward factor depth pen

        '|' ->
            moveForward factor depth pen

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


type alias Axiom =
    String


type alias Rules =
    List ( Char, String )


type alias Config =
    { axiom : Axiom
    , rules : Rules
    , origin : Vec
    , initialLength : Float
    , initialAngle : Float
    , deltaAngle : Float
    , stepSize : Float
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
