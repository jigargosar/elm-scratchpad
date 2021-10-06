module Particles exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes.InPx as Px


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Particle =
    { x : Float
    , y : Float
    , r : Float
    , vx : Float
    , vy : Float
    }


randomParticle : Generator Particle
randomParticle =
    Random.map5 Particle
        (Random.float 0 width)
        (Random.float 0 height)
        --(Random.float 1 8)
        (Random.float 1 4)
        (Random.float -2 2)
        (Random.float -1 1.5)


updateParticle : Particle -> Particle
updateParticle =
    moveByVelocity >> bounceAgainstScreenEdge


moveByVelocity : Particle -> Particle
moveByVelocity ({ x, y, vx, vy } as p) =
    { p | x = x + vx, y = y + vy }


bounceAgainstScreenEdge : Particle -> Particle
bounceAgainstScreenEdge ({ x, y, vx, vy } as p) =
    { p
        | x = clamp 0 width x
        , y = clamp 0 height y
        , vx =
            if x < 0 || x > width then
                negate vx

            else
                vx
        , vy =
            if y < 0 || y > height then
                negate vy

            else
                vy
    }


type alias Model =
    { particles : List Particle }


init : () -> ( Model, Cmd Msg )
init () =
    let
        ( particles, _ ) =
            Random.step
                (Random.list (width // 10) randomParticle)
                (Random.initialSeed 0)
    in
    ( { particles = particles
      }
    , Cmd.none
    )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( { model
                | particles =
                    model.particles
                        |> List.map updateParticle
              }
            , Cmd.none
            )


width =
    500


height =
    500


view : Model -> Html Msg
view model =
    Svg.svg
        [ SA.width (String.fromFloat width)
        , SA.height (String.fromFloat height)
        , SA.fill "none"
        , SA.stroke "none"
        , style "background-color" "#333"
        , style "background-color" "#0f0f0f"
        ]
        [ model.particles
            |> List.map viewParticle
            |> Svg.g []
        ]


type alias Point =
    ( Float, Float )


type alias Seg =
    ( Point, Point )


createConnections : List Particle -> List Seg -> List Seg
createConnections pendingParticles connections =
    case pendingParticles of
        [] ->
            connections

        p :: ps ->
            createConnections ps (createConnectionsHelp p ps ++ connections)


createConnectionsHelp : Particle -> List Particle -> List Seg
createConnectionsHelp p ps =
    Debug.todo "impl"


viewParticle : Particle -> Svg msg
viewParticle { x, y, r } =
    Svg.circle
        [ Px.cx x
        , Px.cy y
        , Px.r r
        , SA.fill "white"
        , SA.fill "rgba(200,169,169,0.5)"
        ]
        []
