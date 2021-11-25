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
    , len : Float
    }


initialRootNode : Node
initialRootNode =
    nodeFromLC 500 vZero


nodeFromLC : Float -> Vec -> Node
nodeFromLC len c =
    { center = c, len = len }


createChildren : Node -> List Node
createChildren node =
    let
        len =
            node.len / 3
    in
    [ vec -len 20, vec len 20 ]
        |> List.map (vAdd node.center >> nodeFromLC len)


drawNode : Node -> Svg msg
drawNode node =
    TS.polyline
        [ TA.points
            [ ( node.center.x - node.len / 2, node.center.y )
            , ( node.center.x + node.len / 2, node.center.y )
            ]
        ]
        []


genCantor : List Node -> List Node -> List Node
genCantor oldPending acc =
    case oldPending of
        [] ->
            acc

        node :: pending ->
            if node.len < 1 then
                genCantor pending acc

            else
                genCantor
                    (createChildren node
                        |> List.foldl cons pending
                    )
                    (node :: acc)
