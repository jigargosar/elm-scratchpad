module Particles exposing (..)

import Browser
import Html exposing (Html)
import Svg
import Svg.Attributes as SA
import Time


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
        ]
        []
