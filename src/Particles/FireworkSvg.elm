module Particles.FireworkSvg exposing (..)

import Browser
import Browser.Events
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import Utils exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { now : Int, particles : List Particle }


init : () -> ( Model, Cmd Msg )
init () =
    ( { now = 0, particles = initialParticles }, Cmd.none )


initialParticles : List Particle
initialParticles =
    Random.step randomParticles (Random.initialSeed 0)
        |> first


type Msg
    = Frame Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame (Time.posixToMillis >> Frame)
            |> always Sub.none
        , Time.every (1000 / 35) (Time.posixToMillis >> Frame)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame now ->
            let
                ds =
                    (toFloat (now - model.now) |> clamp 0 100)
                        / 1000
            in
            ( { model
                | now = now
                , particles = updatePS ds model.particles
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        fr =
            secondsToFractionOverNowMills 3 model.now
    in
    svg
        [ viewBoxC 300 300
        , dBlock
        , noFill
        , noStroke
        , bgc black
        ]
        [ group [] (List.map (viewParticle fr) model.particles)
        ]


type alias Seconds =
    Float


type alias Particle =
    { nv : Vec
    , h : Float
    , ip : Vec
    , p : Vec
    , v : Vec
    , maxLifetimeS : Seconds
    , lifetimeS : Seconds
    }


initParticle : Vec -> Float -> Particle
initParticle nv h =
    let
        iv =
            vScale 50 nv

        originP =
            vZero

        initialDisplacement =
            vScale 0 iv

        initialPosition =
            vAdd originP initialDisplacement
    in
    { nv = nv
    , h = h
    , ip = initialPosition
    , p = initialPosition
    , v = iv
    , maxLifetimeS = 2
    , lifetimeS = 0
    }


updatePS : Float -> List Particle -> List Particle
updatePS ds ps =
    let
        nps =
            List.filterMap (particleStep ds) ps
    in
    if nps == [] then
        initialParticles

    else
        nps


particleStep : Seconds -> Particle -> Maybe Particle
particleStep ds pa =
    if pa.lifetimeS + ds > pa.maxLifetimeS then
        Nothing

    else
        let
            cVel =
                vScale ds pa.v

            cDragVel =
                cVel |> vScale -0.1
        in
        Just
            { pa
                | lifetimeS = pa.lifetimeS + ds
                , p = vAdd pa.p cVel
                , v = vAdd pa.v cDragVel
            }


randomParticles : Generator (List Particle)
randomParticles =
    let
        gen =
            Random.map2 initParticle
                (Random.pair (randomNorm |> Random.map (\x -> sqrt x)) randomAngle
                    |> Random.map vFromPolar
                )
                randomHue
    in
    Random.list 70 gen


viewParticle : Float -> Particle -> Svg msg
viewParticle _ ({ nv, h } as pa) =
    let
        vInitial =
            nv |> vScale (maxLen * 0.1)

        maxLen =
            100

        nl =
            pa.lifetimeS / pa.maxLifetimeS
    in
    viewTrail h
        vInitial
        pa.p
        [ SA.opacity <| fromFloat <| rangeMap ( 0.2, 1 ) ( 1, 0 ) nl
        ]


randomHue : Generator Float
randomHue =
    Random.float 0 1


viewTrail h s e aa =
    normSamples 8
        |> List.map
            (\n ->
                let
                    p =
                        vLerp s e n
                in
                circle 1
                    [ fill <| hsla h 1 0.5 (n * n * n)
                    , xf [ mv p ]
                    ]
            )
        |> group aa
