module TCBON.LSystemV2 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Svg exposing (Svg)
import TypedSvg.Attributes as TA
import Utils exposing (..)


main =
    let
        bigH : Config
        bigH =
            { axiom = "[F]--F"
            , rules = [ ( 'F', "|[+F][-F]" ) ]
            , deltaAngle = degrees 90
            , stepSize = 13 / 20
            , initialAngle = 0
            }

        bentBigH : Config
        bentBigH =
            { axiom = "[F]--F"
            , rules = [ ( 'F', "|[+F][-F]" ) ]
            , deltaAngle = degrees 80
            , stepSize = 13 / 20
            , initialAngle = degrees -5
            }

        twoYs : Config
        twoYs =
            { axiom = "[F]----F"
            , rules = [ ( 'F', "|[+F][-F]" ) ]
            , deltaAngle = degrees 45
            , stepSize = 13 / 20
            , initialAngle = 0
            }

        twig : Config
        twig =
            { axiom = "F"
            , rules = [ ( 'F', "|[-F][+F]" ) ]
            , deltaAngle = degrees 20
            , stepSize = 1 / 2
            , initialAngle = 0
            }

        weed_1 : Config
        weed_1 =
            { axiom = "F"
            , rules = [ ( 'F', "F[-F]F[+F]F" ) ]
            , deltaAngle = degrees 25
            , stepSize = 1 / 2
            , initialAngle = 0
            }

        weed_2 : Config
        weed_2 =
            { axiom = "F"
            , rules = [ ( 'F', "|[-F]|[+F]F" ) ]
            , deltaAngle = degrees 25
            , stepSize = 1 / 2.5
            , initialAngle = 0
            }

        weed_3 : Config
        weed_3 =
            { axiom = "F"
            , rules = [ ( 'F', "|[-F]|[+F][-F]F" ) ]
            , deltaAngle = degrees 20
            , stepSize = 1 / 3
            , initialAngle = 0
            }

        bush_1 : Config
        bush_1 =
            { axiom = "F"
            , rules = [ ( 'F', "FF+[+F-F-F]-[-F+F+F]" ) ]
            , deltaAngle = degrees 25
            , stepSize = 1 / 2
            , initialAngle = 0
            }

        bush_2 : Config
        bush_2 =
            { axiom = "F"
            , rules = [ ( 'F', "|[+F]|[-F]+F" ) ]
            , deltaAngle = degrees 20
            , stepSize = 1 / 2
            , initialAngle = 0
            }

        tree_1 : Config
        tree_1 =
            { axiom = "F"
            , rules = [ ( 'F', "|[---F][+++F]|[--F][++F]|F" ) ]
            , deltaAngle = degrees 20
            , stepSize = 1 / 2
            , initialAngle = 0
            }

        tree_2 : Config
        tree_2 =
            { axiom = "F"
            , rules = [ ( 'F', "|[+++++F][-------F]-|[++++F][------F]-|[+++F][-----F]-|F" ) ]
            , deltaAngle = degrees 8
            , stepSize = 1 / 2
            , initialAngle = 0
            }
    in
    div []
        ([ ( bigH, 9 )
         , ( bentBigH, 9 )
         , ( twoYs, 9 )
         , ( twig, 9 )
         , ( weed_1, 5 )
         , ( weed_2, 6 )
         , ( weed_3, 5 )
         , ( bush_1, 5 )
         , ( bush_2, 8 )
         , ( tree_1, 6 )
         , ( tree_2, 5 )
         ]
            |> List.map viewLSys3
        )


viewLSys3 ( c, d ) =
    div
        [ dGrid
        , style "grid-auto-flow" "column"
        , style "justify-items" "stretch"

        --, style "align-items" "stretch"
        ]
        [ lsys c 1, lsys c 2, lsys c d ]


type alias Segment =
    ( Vec, Vec )


type alias Bounds =
    { min : Vec, max : Vec }


addPointToBounds : Vec -> Bounds -> Bounds
addPointToBounds vec bounds =
    { min = vMap2 min bounds.min vec
    , max = vMap2 max bounds.max vec
    }


render : Config -> List C2 -> Html msg
render config chs =
    let
        ( bounds, drawing ) =
            List.foldl renderChar ( initTurtle config, [] ) chs
                |> second
                |> List.foldl
                    (\( a, b ) ( bnd, acc ) ->
                        ( bnd
                            |> addPointToBounds a
                            |> addPointToBounds b
                        , vPolyline [ a, b ] [ style "vector-effect" "non-scaling-stroke" ]
                            :: acc
                        )
                    )
                    ( { min = vZero, max = vZero }, [] )
                |> mapFirst
                    (\b ->
                        { min = vMapBoth (floor >> toFloat) b.min
                        , max = vMapBoth (ceiling >> toFloat) b.max
                        }
                    )

        vDiff =
            vSub bounds.max bounds.min
    in
    Svg.svg
        [ TA.viewBox (bounds.min.x - 5) (bounds.min.y - 5) (vDiff.x + 10) (vDiff.y + 10)
        , style "max-height" "200px"
        , dBlock
        , noFill
        , noStroke
        , overflowHidden
        , style "outline" "auto blue"
        ]
        [ group
            [ strokeW 1
            , stroke black
            ]
            drawing
        ]
        |> Svg.map never


moveForward : Int -> ( Turtle, List Segment ) -> ( Turtle, List Segment )
moveForward t ( pen, acc ) =
    let
        np =
            vAdd pen.p (vFromPolar ( pen.len * (pen.ds ^ toFloat t), pen.a ))
    in
    ( { pen | p = np }
    , ( pen.p, np ) :: acc
    )


renderChar : C2 -> ( Turtle, List Segment ) -> ( Turtle, List Segment )
renderChar (C2 depth c) (( t, acc ) as tAcc) =
    case c of
        'F' ->
            moveForward depth tAcc

        '|' ->
            moveForward depth tAcc

        '-' ->
            ( { t | a = t.a - t.da }, acc )

        '+' ->
            ( { t | a = t.a + t.da }, acc )

        '[' ->
            ( { t | prev = Prev t }, acc )

        ']' ->
            ( case t.prev of
                None ->
                    t

                Prev pt ->
                    pt
            , acc
            )

        _ ->
            ( t, acc )


type alias Turtle =
    { p : Vec
    , a : Float
    , da : Float
    , ds : Float
    , len : Float
    , prev : Prev
    }


initTurtle : Config -> Turtle
initTurtle config =
    { p = vZero

    --p = config.origin
    , a = degrees -90 + config.initialAngle
    , da = config.deltaAngle
    , ds = config.stepSize

    --, len = config.initialLength
    , len = 100
    , prev = None
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
