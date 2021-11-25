module Fractals.CH_08_04_CantorSet exposing (..)

import Svg exposing (Svg)
import TypedSvg as TS
import TypedSvg.Attributes as TA
import Utils exposing (..)


main =
    Svg.svg
        [ style "width" "100vw"
        , style "height" "100vh"
        , dBlock
        , noFill
        , noStroke
        ]
        [ group
            [ style "transform" "translate(50%,30%)"
            , strokeW 10
            , stroke black
            ]
            [ genCantor [ initialRootNode ] []
                |> List.map drawNode
                |> group []
            ]
        ]


type alias Node =
    { center : Vec
    , diameter : Float
    }


initialRootNode : Node
initialRootNode =
    nodeFromLC 250 vZero


nodeFromLC : Float -> Vec -> Node
nodeFromLC len c =
    { center = c, diameter = len }


createChildren : Node -> List Node
createChildren node =
    let
        oldRadius =
            node.diameter / 2

        radius =
            oldRadius / 3

        diameter =
            2 * radius

        xOffSet =
            oldRadius * 2 / 3
    in
    [ vec -xOffSet 20, vec xOffSet 20 ]
        |> List.map (vAdd node.center >> nodeFromLC diameter)


drawNode : Node -> Svg msg
drawNode node =
    TS.polyline
        [ TA.points
            [ ( node.center.x - node.diameter / 2, node.center.y )
            , ( node.center.x + node.diameter / 2, node.center.y )
            ]
        ]
        []


genCantor : List Node -> List Node -> List Node
genCantor oldPending acc =
    case oldPending of
        [] ->
            acc

        node :: pending ->
            if node.diameter < 1 then
                genCantor pending acc

            else
                genCantor
                    (createChildren node
                        |> List.foldl cons pending
                    )
                    (node :: acc)
