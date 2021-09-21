module Main exposing (main)

import Html exposing (text)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT


main =
    Svg.svg [ style "font-size" "20px" ]
        [ Svg.text_
            [ Px.x 10
            , Px.y 10
            ]
            [ text "hi" ]
        , drawLine ( 10, 10 ) ( 50, 50 ) "0.1"
        , drawLine ( 50, 50 ) ( 150, 10 ) "0.1"
        , drawLines
            [ ( 10, 10 )
            , ( 50, 50 )
            , ( 50, 10 )
            , ( 60, 50 )
            , ( 70, 10 )
            , ( 80, 50 )
            , ( 90, 10 )
            , ( 100, 50 )
            ]
        , Random.step randomWalk (Random.initialSeed 0)
            |> (\( pts, _ ) -> drawLines pts)
        ]


type alias Point =
    ( Float, Float )


randomWalk : Generator (List Point)
randomWalk =
    Debug.todo "impl"


drawLines pts =
    let
        len =
            toFloat (List.length pts)

        foo i ( a, b ) =
            drawLine_V2 a b ((toFloat i + 1) / len)

        lines =
            List.map2 Tuple.pair pts (List.drop 1 pts)
                |> List.indexedMap foo
    in
    Svg.g [] lines


drawLine_V2 a b o =
    Svg.polyline
        [ TA.points [ a, b ]
        , SA.stroke "black"
        , TA.opacity (TT.Opacity o)
        ]
        []


drawLine a b o =
    Svg.polyline
        [ TA.points [ a, b ]
        , SA.stroke "black"
        , SA.opacity o
        ]
        []
