module Particles.FireworkSMIL exposing (main)

import Random
import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT
import Utils exposing (..)


main =
    svgBlock
        [ viewBoxC 300 300
        , sMaxWidth "500px"
        ]
        [ particles
            |> List.map viewParticle
            |> group []
        ]


type alias Particle =
    { to : Float2 }


particles =
    let
        gen =
            Random.map Particle
                (Random.constant ( 100, -100 ))
    in
    Random.step (Random.list 100 gen) (Random.initialSeed 0)
        |> first


viewParticle { to } =
    let
        refIdAttr =
            SA.id "a_mv"

        beginAttr =
            SA.begin "0s;a_mv.end+0.5s"

        durAttr =
            SA.dur "2s"
    in
    Svg.circle [ Px.r 2, fill <| hsl 1 1 0.5 ]
        [ Svg.animateTransform
            [ refIdAttr
            , SA.attributeName "transform"
            , SA.type_ "translate"
            , valuesFloat2 [ ( 0, 0 ), to ]
            , durAttr
            , beginAttr
            , SA.fill "freeze"
            ]
            []
        , Svg.animate
            [ SA.attributeName "opacity"
            , SA.values "1;1;0"
            , SA.keyTimes "0;0.7;1"
            , beginAttr
            , durAttr
            , SA.fill "freeze"
            ]
            []
        ]


valuesFloat2 : List Float2 -> Svg.Attribute msg
valuesFloat2 =
    valuesFromFloat2List >> SA.values


valuesFromFloat2List : List Float2 -> String
valuesFromFloat2List =
    List.map (joinFloat2 " ") >> String.join ";"


join sep ( a, b ) =
    a ++ sep ++ b


joinFloat2 sep =
    mapEach fromFloat >> join sep
