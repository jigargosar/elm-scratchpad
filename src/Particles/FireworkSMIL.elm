module Particles.FireworkSMIL exposing (main)

import Random exposing (Generator)
import Svg
import Svg.Attributes as SA
import Tuple exposing (pair)
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
    { to : Float2
    , h : Float
    }


particles : List Particle
particles =
    let
        randomDest =
            Random.pair (Random.float 10 100) randomAngle
                |> Random.map fromPolar

        gen : Generator Particle
        gen =
            Random.map2 Particle
                randomDest
                randomNorm
    in
    Random.step (Random.list 100 gen) (Random.initialSeed 0)
        |> first


viewParticle : Particle -> Svg msg
viewParticle { to, h } =
    let
        refIdAttr =
            SA.id "a_mv"

        beginAttr =
            SA.begin "0s;a_mv.end+0.5s"

        durAttr =
            SA.dur "2s"
    in
    Svg.circle [ Px.r 2, fill <| hsl h 1 0.5 ]
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
