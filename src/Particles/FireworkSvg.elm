module Particles.FireworkSvg exposing (..)

import Browser
import Browser.Events
import Random exposing (Generator)
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
                    (toFloat (now - model.now) |> clamp 0 200)
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
    svg
        [ viewBoxC 300 300
        , dBlock
        , noFill
        , noStroke
        , bgc black
        ]
        [ group [] (List.map viewParticle model.particles)
        ]


type alias Seconds =
    Float


type alias Particle =
    { nv : Vec
    , h : Float
    , ip : Vec
    , p : Vec
    , oldPS : List Vec
    , v : Vec
    , maxLifetimeS : Seconds
    , lifetimeS : Seconds
    }


initParticle : Vec -> Float -> Particle
initParticle nv h =
    let
        iv =
            vScale 150 nv

        originPosition =
            vZero

        initialDisplacement =
            vScale 0.1 iv

        initialPosition =
            vAdd originPosition initialDisplacement
    in
    { nv = nv
    , h = h
    , ip = initialPosition
    , p = initialPosition
    , oldPS = [ initialPosition ]
    , v = iv
    , maxLifetimeS = 1
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
    if pa.lifetimeS + ds > pa.maxLifetimeS || (pa.v |> vToPolar |> first) < 5 then
        Nothing

    else
        let
            cVel =
                vScale ds pa.v

            cDragVel =
                cVel |> vScale -2
        in
        Just
            { pa
                | lifetimeS = pa.lifetimeS + ds
                , p = vAdd pa.p cVel
                , v = vAdd pa.v cDragVel
                , oldPS = pa.p :: pa.oldPS |> List.take 5
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
    Random.list 200 gen


viewParticle : Particle -> Svg msg
viewParticle ({ nv, h } as pa) =
    let
        nl =
            pa.lifetimeS / pa.maxLifetimeS

        lifetimeOpacity =
            clamp 0 1 <| rangeMap ( 0.7, 1 ) ( 1, 0 ) nl

        vt =
            viewTrail h
                pa.ip
                pa.p
                [ SA.opacity <| fromFloat <| lifetimeOpacity
                ]

        vl =
            vPolyline (pa.p :: pa.oldPS)
                [ stroke <|
                    hsla h
                        1
                        0.5
                        (clamp 0 1 <|
                            rangeMap ( 10, 5 )
                                ( 1, 0 )
                                (pa.v |> vToPolar |> first)
                        )
                , strokeW 2
                , xf [ mv pa.p ]
                ]
    in
    vt
        |> always vl



--circle (pa.v |> vToPolar |> first |> mul 0.05)
--    [ xf [ mv pa.p ]
--    , fill <| hsla h 1 0.5 1
--    , SA.opacity <| fromFloat <| lifetimeOpacity
--    ]


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
