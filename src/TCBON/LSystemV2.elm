module TCBON.LSystemV2 exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Regex
import Svg exposing (Svg)
import TypedSvg.Attributes as TA
import Utils exposing (..)


bigH : Config
bigH =
    { axiom = "[F]--F"
    , rules = [ ( 'F', "|[+F][-F]" ) ]
    , angle = degrees 90
    , stepSize = 13 / 20
    , initialAngle = 0
    }


bentBigH : Config
bentBigH =
    { axiom = "[F]--F"
    , rules = [ ( 'F', "|[+F][-F]" ) ]
    , angle = degrees 80
    , stepSize = 13 / 20
    , initialAngle = degrees -5
    }


twoYs : Config
twoYs =
    { axiom = "[F]----F"
    , rules = [ ( 'F', "|[+F][-F]" ) ]
    , angle = degrees 45
    , stepSize = 13 / 20
    , initialAngle = 0
    }


twig : Config
twig =
    { axiom = "F"
    , rules = [ ( 'F', "|[-F][+F]" ) ]
    , angle = degrees 20
    , stepSize = 1 / 2
    , initialAngle = 0
    }


weed_1 : Config
weed_1 =
    { axiom = "F"
    , rules = [ ( 'F', "F[-F]F[+F]F" ) ]
    , angle = degrees 25
    , stepSize = 1 / 2
    , initialAngle = 0
    }


weed_2 : Config
weed_2 =
    { axiom = "F"
    , rules = [ ( 'F', "|[-F]|[+F]F" ) ]
    , angle = degrees 25
    , stepSize = 1 / 2.5
    , initialAngle = 0
    }


weed_3 : Config
weed_3 =
    { axiom = "F"
    , rules = [ ( 'F', "|[-F]|[+F][-F]F" ) ]
    , angle = degrees 20
    , stepSize = 1 / 3
    , initialAngle = 0
    }


bush_1 : Config
bush_1 =
    { axiom = "F"
    , rules = [ ( 'F', "FF+[+F-F-F]-[-F+F+F]" ) ]
    , angle = degrees 25
    , stepSize = 1 / 2
    , initialAngle = 0
    }


bush_2 : Config
bush_2 =
    { axiom = "F"
    , rules = [ ( 'F', "|[+F]|[-F]+F" ) ]
    , angle = degrees 20
    , stepSize = 1 / 2
    , initialAngle = 0
    }


tree_1 : Config
tree_1 =
    { axiom = "F"
    , rules = [ ( 'F', "|[---F][+++F]|[--F][++F]|F" ) ]
    , angle = degrees 20
    , stepSize = 1 / 2
    , initialAngle = 0
    }


tree_2 : Config
tree_2 =
    { axiom = "F"
    , rules = [ ( 'F', "|[+++++F][-------F]-|[++++F][------F]-|[+++F][-----F]-|F" ) ]
    , angle = degrees 8
    , stepSize = 1 / 2
    , initialAngle = 0
    }


tree_3 : Config
tree_3 =
    { axiom = "F"
    , rules = [ ( 'F', "|[--F][+F]-F" ) ]
    , angle = degrees 20
    , stepSize = 1 / 1.5
    , initialAngle = 0
    }


carpet : Config
carpet =
    { axiom = "F-F-F-F"
    , rules = [ ( 'F', "F[F]-F+F[--F]+F-F" ) ]
    , angle = degrees 90
    , stepSize = 1 / 2
    , initialAngle = 0
    }


sierpinskiSquare : Config
sierpinskiSquare =
    { axiom = "F-F-F-F"
    , rules = [ ( 'F', "FF[-F-F-F]F" ) ]
    , angle = degrees 90
    , stepSize = 1 / 2
    , initialAngle = 0
    }


rug : Config
rug =
    { axiom = "F-F-F-F"
    , rules = [ ( 'F', "F[-F-F]FF" ) ]
    , angle = degrees 90
    , stepSize = 1 / 2
    , initialAngle = 0
    }


kochIsland : Config
kochIsland =
    { axiom = "F++F++F"
    , rules = [ ( 'F', "F-F++F-F" ) ]
    , angle = degrees 60
    , stepSize = 1 / 2
    , initialAngle = degrees 90
    }


quadraticKochIsland : Config
quadraticKochIsland =
    { axiom = "F-F-F-F"
    , rules = [ ( 'F', "F-F+F+FF-F-F+F" ) ]
    , angle = degrees 90
    , stepSize = 1 / 2
    , initialAngle = degrees 0
    }


squareSpikes : Config
squareSpikes =
    { axiom = "F18-F18-F18-F"
    , rules = [ ( 'F', "F17-F34+F17-F" ) ]
    , angle = degrees 5
    , stepSize = 1 / 2
    , initialAngle = degrees 0
    }


sierpinskiGasket : Config
sierpinskiGasket =
    { axiom = "F--F--F"
    , rules =
        [ ( 'F', "F--F--F--GG" )
        , ( 'G', "GG" )
        ]
    , angle = degrees 60
    , stepSize = 1 / 2
    , initialAngle = degrees 90
    }


sierpinskiMaze : Config
sierpinskiMaze =
    { axiom = "F"
    , rules =
        [ ( 'F', "[GF][+G3-F][G+G+F]" )
        , ( 'G', "GG" )
        ]
    , angle = degrees 60
    , stepSize = 1 / 2
    , initialAngle = degrees 30
    }


sierpinskiArrowHead : Config
sierpinskiArrowHead =
    { axiom = "F"
    , rules =
        [ ( 'F', "[-G+++F][-G+F][GG--F]" )
        , ( 'G', "GG" )
        ]
    , angle = degrees 60
    , stepSize = 1 / 2
    , initialAngle = degrees 90
    }


penroseTile : Config
penroseTile =
    { axiom = "[X]++[X]++[X]++[X]++[X]"
    , rules =
        [ ( 'W', "YF++ZF4-XF[-YF4-WF]++" )
        , ( 'X', "+YF--ZF[3-WF--XF]+" )
        , ( 'Y', "-WF++XF[+++YF++ZF]-" )
        , ( 'Z', "--YF++++WF[+ZF++++XF]--XF" )
        , ( 'F', "" )
        ]
    , angle = degrees 36
    , stepSize = 1
    , initialAngle = degrees 0
    }


penroseSnowflake : Config
penroseSnowflake =
    { axiom = "F4-F4-F4-F4-F"
    , rules = [ ( 'F', "F4-F4-F10-F++F4-F" ) ]
    , angle = degrees 18
    , stepSize = 1 / 2
    , initialAngle = degrees 0
    }


dragonCurve : Config
dragonCurve =
    { axiom = "F"
    , rules =
        [ ( 'F', "[+F][+G--G4-F]" )
        , ( 'G', "-G++G-" )
        ]
    , angle = degrees 45
    , stepSize = 1 / 1.25
    , initialAngle = degrees 90
    }



--noinspection ElmUnusedSymbol


dragonCurveBracketLess : Config
dragonCurveBracketLess =
    { axiom = "Fl"
    , rules =
        [ ( 'l', "l+rF+" )
        , ( 'r', "-Fl-r" )
        ]
    , angle = degrees 90
    , stepSize = 1
    , initialAngle = degrees 45
    }



--noinspection ElmUnusedSymbol


part1 =
    [ ( bigH, 9 )
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
    , ( tree_3, 9 )
    , ( carpet, 5 )
    , ( sierpinskiSquare, 5 )
    , ( rug, 5 )
    , ( kochIsland, 6 )
    , ( quadraticKochIsland, 4 )
    , ( squareSpikes, 6 )
    , ( sierpinskiGasket, 6 )
    , ( sierpinskiMaze, 7 )
    , ( sierpinskiArrowHead, 7 )
    ]
        |> List.map viewLSys_1_2_N


part2 =
    [ viewLSys_1_2_N ( penroseSnowflake, 5 )
    , viewLSys3 ( penroseTile, ( 2, 3, 7 ) )
    , viewLSys_1_2_N ( dragonCurve, 14 )

    --, viewLSys_1_2_N ( dragonCurveBracketLess, 14 )
    ]


main =
    div []
        (part2
         --++ part1
        )


viewLSys_1_2_N ( c, d ) =
    viewLSys3 ( c, ( 1, 2, d ) )


viewLSys3 ( c, ( d1, d2, d3 ) ) =
    div
        [ displayGrid
        , style "grid-auto-flow" "column"
        , style "justify-items" "stretch"

        --, style "align-items" "stretch"
        ]
        [ lsys c d1, lsys c d2, lsys c d3 ]


type alias Segment =
    ( Vec, Vec )


type alias Bounds =
    { min : Vec, max : Vec }


addPointToBounds : Vec -> Bounds -> Bounds
addPointToBounds vec bounds =
    { min = vMap2 min bounds.min vec
    , max = vMap2 max bounds.max vec
    }


render : Config -> Int -> List C2 -> Html msg
render config maxDepth chs =
    let
        ( bounds, drawing ) =
            List.foldl (renderChar maxDepth) ( initTurtle config, [] ) chs
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
                        { min = vMapEach (floor >> toFloat) b.min
                        , max = vMapEach (ceiling >> toFloat) b.max
                        }
                    )

        vDiff =
            vSub bounds.max bounds.min

        borderW =
            5
    in
    Svg.svg
        [ TA.viewBox
            (bounds.min.x - borderW)
            (bounds.min.y - borderW)
            (vDiff.x + borderW * 2)
            (vDiff.y + borderW * 2)
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
moveForward depth ( t, acc ) =
    let
        np =
            vAdd t.p (vFromPolar ( t.len * (t.ds ^ toFloat depth), t.a ))
    in
    ( { t | p = np }
    , ( t.p, np ) :: acc
    )


renderChar : Int -> C2 -> ( Turtle, List Segment ) -> ( Turtle, List Segment )
renderChar _ (C2 depth c) (( t, acc ) as tAcc) =
    case c of
        'F' ->
            moveForward depth tAcc

        '|' ->
            moveForward depth tAcc

        'G' ->
            moveForward depth tAcc
                |> mapSecond (withRollback List.tail)

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
    , da = config.angle
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
    , angle : Float
    , stepSize : Float
    }


type C2
    = C2 Int Char


expandAxiom : String -> List C2
expandAxiom =
    expandString 0


expandString : Int -> String -> List C2
expandString depth =
    preprocessRule >> String.toList >> List.map (C2 depth)


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
        |> render config maxDepth


preprocessRule : String -> String
preprocessRule =
    let
        digitsFollowedByPlusOrMinus =
            Regex.fromString "\\d+[+-]"
                |> Maybe.withDefault Regex.never

        expandPrefixedAngle m =
            String.repeat
                (m.match
                    |> String.dropRight 1
                    |> String.toInt
                    |> Maybe.withDefault 0
                )
                (String.slice (String.length m.match - 1) (String.length m.match) m.match)
    in
    Regex.replace digitsFollowedByPlusOrMinus expandPrefixedAngle
