module Fractals.CH_08_01_CantorSetExercise exposing (..)

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
            [ style "transform" "translate(50%,50%)"
            , strokeW 1
            , stroke black
            ]
            [ genCantor [ initialRootNode ] []
                |> List.map drawNode
                |> group []
            ]
        ]


type alias Node =
    { horizontal : Bool
    , center : Vec
    , radius : Float
    }


initialRootNode : Node
initialRootNode =
    nodeFromRC True 300 vZero


nodeFromRC : Bool -> Float -> Vec -> Node
nodeFromRC b r c =
    { horizontal = b, center = c, radius = r }


createChildren : Node -> List Node
createChildren node =
    let
        radius =
            node.radius / 1.5

        offset =
            radius * 1.5
    in
    (if node.horizontal then
        [ vec -offset 0, vec offset 0 ]

     else
        [ vec 0 -offset, vec 0 offset ]
    )
        |> List.map (vAdd node.center >> nodeFromRC (not node.horizontal) radius)


drawNode : Node -> Svg msg
drawNode node =
    let
        center =
            node.center
    in
    TS.polyline
        [ TA.points
            (if node.horizontal then
                [ ( center.x - node.radius, center.y )
                , ( center.x + node.radius, center.y )
                ]

             else
                [ ( center.x, center.y - node.radius )
                , ( center.x, center.y + node.radius )
                ]
            )
        ]
        []


genCantor : List Node -> List Node -> List Node
genCantor oldPending acc =
    case oldPending of
        [] ->
            acc

        node :: pending ->
            if node.radius < 1.5 then
                genCantor pending acc

            else
                genCantor
                    (createChildren node
                        |> List.foldl cons pending
                    )
                    (node :: acc)
