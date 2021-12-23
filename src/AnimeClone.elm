module AnimeClone exposing (main)

import Browser.Events
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { animClock : Int }


init : () -> ( Model, Cmd Msg )
init () =
    ( { animClock = 0 }, Cmd.none )


type Msg
    = NOP
    | OnAnimationFrameDeltaMilli Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (round >> OnAnimationFrameDeltaMilli)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnAnimationFrameDeltaMilli animationFrameDeltaMilli ->
            ( { model
                | animClock = model.animClock + clamp 0 100 animationFrameDeltaMilli
              }
            , Cmd.none
            )


animateParticle : Int -> Int -> Particle -> Particle
animateParticle start now particle =
    particle
        |> computeAnimated cyclesAnimConfig start now
        |> computeAnimated chargeAnimConfig start now


view : Model -> Document Msg
view model =
    Document "App Title"
        [ basicStylesNode
        , let
            p : Particle
            p =
                animateParticle 0 model.animClock initialParticle
          in
          div [] [ viewParticle p ]
        ]


type alias Particle =
    { charge : String
    , cycles : Int
    }


initialParticle : Particle
initialParticle =
    { charge = "0%", cycles = 120 }


cyclesAnimConfig : AnimConfig Particle Int
cyclesAnimConfig =
    { from = .obj >> .cycles
    , to = always 130
    , duration = always 1800
    , delay = always 0
    , setter = \v o -> { o | cycles = v }
    , interpolator = lerpInt
    }


chargeAnimConfig : AnimConfig Particle String
chargeAnimConfig =
    { from = .obj >> .charge
    , to = always "100%"
    , duration = always 1800
    , delay = always 0
    , setter = \v o -> { o | charge = v }
    , interpolator = lerpPctString
    }


lerpPctString : String -> String -> Float -> String
lerpPctString a b =
    let
        toInt =
            String.slice 0 -1 >> String.toInt >> Maybe.withDefault 0
    in
    lerpInt (toInt a) (toInt b) >> String.fromInt >> (\s -> s ++ "%")


computeAnimated : AnimConfig o v -> Int -> Int -> o -> o
computeAnimated config start now obj =
    let
        args : Args o
        args =
            Args obj 0 1

        duration =
            config.duration args

        delay =
            config.delay args

        frac =
            (toFloat (now - (start + delay)) / toFloat duration)
                |> clamp 0 1

        value =
            config.interpolator (config.from args) (config.to args) frac
    in
    config.setter value obj


type alias Args o =
    { obj : o
    , index : Int
    , length : Int
    }


type alias AnimConfig o v =
    { from : Args o -> v
    , to : Args o -> v
    , duration : Args o -> Int
    , delay : Args o -> Int
    , setter : v -> o -> o
    , interpolator : v -> v -> Float -> v
    }


viewParticle : Particle -> Html msg
viewParticle p =
    div []
        [ div [] [ text p.charge ]
        , div [] [ text <| fromInt p.cycles ]
        ]
