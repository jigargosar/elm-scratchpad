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
    { start : Vec
    , len : Float
    }


initialRootNode : Node
initialRootNode =
    nodeFromLC 500 vZero


nodeFromLC : Float -> Vec -> Node
nodeFromLC len c =
    { start = vec (c.x - (len / 2)) c.y, len = len }


createChildren : Node -> List Node
createChildren node =
    let
        s =
            node.start
    in
    [ { node
        | start = vec s.x (s.y + 20)
        , len = node.len / 3
      }
    , { node
        | start = vec (s.x + (node.len * 2 / 3)) (s.y + 20)
        , len = node.len / 3
      }
    ]


drawNode : Node -> Svg msg
drawNode node =
    TS.polyline
        [ TA.points
            [ ( node.start.x, node.start.y )
            , ( node.start.x + node.len, node.start.y )
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
