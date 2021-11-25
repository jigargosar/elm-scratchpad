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
    { start = c, len = len }


createChildren : Node -> List Node
createChildren node =
    let
        s =
            node.start

        len =
            node.len / 3
    in
    [ { node
        | start = vec (s.x - len) (s.y + 20)
        , len = len
      }
    , { node
        | start = vec (s.x + len) (s.y + 20)
        , len = len
      }
    ]


drawNode : Node -> Svg msg
drawNode node =
    TS.polyline
        [ TA.points
            [ ( node.start.x - node.len / 2, node.start.y )
            , ( node.start.x + node.len / 2, node.start.y )
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
