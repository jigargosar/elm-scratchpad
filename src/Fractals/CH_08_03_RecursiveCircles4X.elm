module Fractals.CH_08_03_RecursiveCircles4X exposing (..)

import Svg exposing (Svg)
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
            [ genCirc [ initialRootNode ] []
                |> List.map drawNode
                |> group []
            ]
        ]


type alias Node =
    { center : Vec
    , radius : Float
    }


nodeFromRC : Float -> Vec -> Node
nodeFromRC r c =
    { center = c, radius = r }


createChildren : Node -> List Node
createChildren node =
    adjacentUnitVectors
        |> List.map
            (vScale node.radius
                >> vAdd node.center
                >> nodeFromRC (node.radius * 0.5)
            )


initialRootNode : Node
initialRootNode =
    nodeFromRC 200 vZero


drawNode : Node -> Svg msg
drawNode node =
    circle node.radius [ xf [ mv node.center ] ]


genCirc : List Node -> List Node -> List Node
genCirc oldPending acc =
    case oldPending of
        [] ->
            acc

        node :: pending ->
            if node.radius < 4 then
                genCirc pending acc

            else
                genCirc
                    (createChildren node
                        |> List.foldl cons pending
                    )
                    (node :: acc)
