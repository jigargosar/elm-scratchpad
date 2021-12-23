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


view : Model -> Document Msg
view model =
    Document "Anime"
        [ basicStylesNode
        , div []
            (initialParticles
                |> List.map (animateParticle 0 model.animClock >> viewParticle)
            )
        ]


type alias Particle =
    { charge : String
    , cycles : Int
    }


initialParticles : List Particle
initialParticles =
    [ { charge = "0%", cycles = 120 } ]


animateParticle : Int -> Int -> Particle -> Particle
animateParticle start now particle =
    particle
        |> computeAnimated chargeAnimConfig start now
        |> computeAnimated cyclesAnimConfig start now


chargeAnimConfig : AnimConfig Particle String
chargeAnimConfig =
    { from = .obj >> .charge
    , to = always "100%"
    , duration = always 1800
    , delay = always 0
    , setter = \v o -> { o | charge = v }
    , interpolator = lerpPctString
    , direction = always Reverse
    , loop = always <| Times 1
    }


cyclesAnimConfig : AnimConfig Particle Int
cyclesAnimConfig =
    { from = .obj >> .cycles
    , to = always 130
    , duration = always 1800
    , delay = always 0
    , setter = \v o -> { o | cycles = v }
    , interpolator = lerpInt
    , direction = always Alternate
    , loop = always Infinite
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

        loop =
            config.loop args

        fr =
            toFloat (now - (start + delay)) / toFloat duration

        frac =
            case loop of
                Infinite ->
                    fr - toFloat (floor fr)

                Times int ->
                    fr |> clamp 0 1

        from =
            config.from args

        to =
            config.to args

        direction =
            config.direction args

        value =
            case direction of
                Normal ->
                    config.interpolator from to frac

                Reverse ->
                    config.interpolator from to (1 - frac)

                Alternate ->
                    config.interpolator from to frac
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
    , direction : Args o -> Direction
    , loop : Args o -> Loop
    }


type Direction
    = Normal
    | Reverse
    | Alternate


type Loop
    = Infinite
    | Times Int


viewParticle : Particle -> Html msg
viewParticle p =
    div []
        [ div [] [ text p.charge ]
        , div [] [ text <| fromInt p.cycles ]
        ]
