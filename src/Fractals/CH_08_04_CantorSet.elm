module Fractals.CH_08_04_CantorSet exposing (..)

import Svg exposing (Svg)
import Utils exposing (..)


width =
    500


height =
    500


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
            [ genCirc2 [ ( 0, 0, 200 ) ] []
                |> List.map (\( x, y, r ) -> circle r [ xf [ mv2 x y ] ])
                |> group []
            , genCirc [ initialRootNode ] []
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
    let
        radius =
            node.radius * 0.5
    in
    [ vec -1 0
    , vec 1 0
    , vec 0 -1
    , vec 0 1
    ]
        |> List.map (vScale radius >> vAdd node.center >> nodeFromRC radius)


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


genCirc2 pending acc =
    case pending of
        [] ->
            acc

        (( x, y, r ) as circleParams) :: newPending ->
            if r < 4 then
                genCirc2 newPending acc

            else
                let
                    rn =
                        r * 0.5
                in
                genCirc2
                    (( x + r, y, rn )
                        :: ( x - r, y, rn )
                        :: ( x, y + r, rn )
                        :: ( x, y - r, rn )
                        :: newPending
                    )
                    (circleParams :: acc)
