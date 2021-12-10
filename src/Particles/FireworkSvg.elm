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
    let
        particles : List Particle
        particles =
            Random.step randomParticles (Random.initialSeed 0)
                |> first
    in
    ( { now = 0, particles = particles }, Cmd.none )


type Msg
    = Frame Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame (Time.posixToMillis >> Frame)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame now ->
            ( { model | now = now }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        fr =
            secondsToFractionOverNowMills 3 model.now
    in
    svg
        [ viewBoxC 300 300
        , noFill
        , noStroke
        , bgc black
        ]
        [ group [] (List.map (viewParticle fr) model.particles)
        ]


type alias Particle =
    ( Vec, Float )


randomParticles : Generator (List Particle)
randomParticles =
    let
        pg =
            Random.pair
                (Random.pair (randomNorm |> Random.map (\x -> sqrt x)) randomAngle
                    |> Random.map vFromPolar
                )
                randomHue
    in
    Random.list 50 pg
        |> Random.map
            (List.sortBy (first >> vLenSquared >> negate))


viewParticle : Float -> Particle -> Svg msg
viewParticle nl ( nv, h ) =
    let
        vInitial =
            nv |> vScale (maxLen * 0.1)

        maxLen =
            100

        e =
            nl
                |> rangeMap ( 0, 0.8 ) ( 0, 1 )
                |> clamp 0 1
                |> vLerp vInitial (vScale maxLen nv)

        s =
            nl
                |> rangeMap ( 0.4, 1 ) ( 0, 1 )
                |> clamp 0 1
                |> vLerp vInitial (vScale maxLen nv)
    in
    viewTrail h
        s
        e
        [ SA.opacity <| fromFloat <| rangeMap ( 0.3, 1 ) ( 1, 0 ) nl
        ]


randomHue : Generator Float
randomHue =
    Random.float 0 1


viewTrail h s e aa =
    normSamples 30
        |> List.map
            (\n ->
                let
                    p =
                        vLerp s e n
                in
                circle 2
                    [ fill <| hsla h 1 0.5 (n * n * n)
                    , xf [ mv p ]
                    ]
            )
        |> group aa
