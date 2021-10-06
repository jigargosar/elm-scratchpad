module Particles exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
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


type alias Model =
    { particles : List Particle }


init : () -> ( Model, Cmd Msg )
init () =
    ( { particles = []
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
            ( model, Cmd.none )


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
        ]
        [ model.particles
            |> List.map viewParticle
            |> Svg.g []
        ]


viewParticle : Particle -> Svg msg
viewParticle { x, y, r } =
    Svg.circle
        [ Px.cx x
        , Px.cy y
        , Px.r r
        , SA.fill "white"
        ]
        []
