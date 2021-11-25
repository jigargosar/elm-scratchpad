module Fractals.CH_08_04_CantorSetEx exposing (..)

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
    , radius : Float
    }


initialRootNode : Node
initialRootNode =
    nodeFromRC 250 vZero


nodeFromRC : Float -> Vec -> Node
nodeFromRC r c =
    { center = c, radius = r }


createChildren : Node -> List Node
createChildren node =
    let
        radius =
            node.radius / 3

        offset =
            radius * 2
    in
    [ vec -offset 20, vec offset 20 ]
        |> List.map (vAdd node.center >> nodeFromRC radius)


drawNode : Node -> Svg msg
drawNode node =
    let
        center =
            node.center
    in
    TS.polyline
        [ TA.points
            [ ( center.x - node.radius, center.y )
            , ( center.x + node.radius, center.y )
            ]
        ]
        []


genCantor : List Node -> List Node -> List Node
genCantor oldPending acc =
    case oldPending of
        [] ->
            acc

        node :: pending ->
            if node.radius < 1 then
                genCantor pending acc

            else
                genCantor
                    (createChildren node
                        |> List.foldl cons pending
                    )
                    (node :: acc)
