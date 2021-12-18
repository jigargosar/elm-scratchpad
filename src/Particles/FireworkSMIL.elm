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
    Random.step (Random.list 20 gen) (Random.initialSeed 0)
        |> first


samples =
    normSamples 10


samplePairs =
    List.map2 pair samples (List.drop 1 samples)


viewParticle : Particle -> Svg msg
viewParticle p =
    samplePairs
        |> List.map (viewTrailPoint p)
        |> group []


viewTrailPoint : Particle -> Float2 -> Svg msg
viewTrailPoint p ( pn, n ) =
    let
        to =
            p.to |> toPolar |> mapFirst (mul n) |> fromPolar

        pto =
            p.to |> toPolar |> mapFirst (mul pn) |> fromPolar

        oa =
            n * n * n

        refIdAttr =
            SA.id "a_particle"

        beginAttr =
            SA.begin "0s;a_particle.end+0.0s"

        durAttr =
            SA.dur "2s"
    in
    --Svg.circle [ Px.r 1, fill <| hsla p.h 1 0.5 oa ]
    Svg.polyline
        [ --TA.points [ pto, to ]
          stroke <| hsla p.h 1 0.5 oa
        ]
        [ --Svg.animateTransform
          --[ refIdAttr
          --, SA.attributeName "transform"
          --, SA.type_ "translate"
          --, valuesFloat2 [ ( 0, 0 ), to ]
          --, durAttr
          --, beginAttr
          --, fill "freeze"
          --]
          --[]
          Svg.animate
            [ refIdAttr
            , SA.attributeName "points"
            , [ [ ( 0, 0 ), ( 0, 0 ) ]
              , [ pto, to ]
              ]
                |> List.map (List.map (joinFloat2 ",") >> String.join " ")
                |> String.join ";"
                |> SA.values

            --, SA.values "0,0 10,10; 0,0 100,100;"
            , durAttr
            , beginAttr
            , fill "freeze"
            ]
            []
        , Svg.animate
            [ SA.attributeName "opacity"

            --, valuesFromFloat [ oa, oa, 0 ]
            , SA.values "1;1;0"
            , SA.keyTimes "0;0.7;1"
            , beginAttr
            , durAttr
            , fill "freeze"
            ]
            []
        ]


valuesFromFloat =
    List.map fromFloat >> String.join ";" >> SA.values


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
