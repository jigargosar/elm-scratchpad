module AnimeClone exposing (main)

import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


type Msg
    = NOP


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )


view : Model -> Document Msg
view _ =
    Document "App Title"
        [ basicStylesNode
        , let
            p : Particle
            p =
                computeAnimated particleAnimationConfig
                    initialParticle
                    0
                    2000
          in
          div [] [ viewParticle p ]
        ]


type alias Particle =
    { charge : String
    , cycles : Int
    }


initialParticle : Particle
initialParticle =
    { charge = "80%", cycles = 120 }


particleAnimationConfig : AnimConfig Particle Int
particleAnimationConfig =
    { from = .obj >> .cycles
    , to = always 130
    , duration = always 1800
    , delay = always 0
    , setter = \v o -> { o | cycles = v }
    , interpolator = lerpInt
    }


computeAnimated : AnimConfig o v -> o -> Int -> Int -> o
computeAnimated config obj start now =
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


viewParticle p =
    text <| fromInt p.cycles
