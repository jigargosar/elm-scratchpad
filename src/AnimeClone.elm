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
                |> List.indexedMap (\i -> animateParticle 0 model.animClock { index = i, length = List.length initialParticles } >> viewParticle)
            )
        ]


type alias Particle =
    { charge : String
    , cycles : Int
    , x : Float
    }


initialParticles : List Particle
initialParticles =
    let
        p =
            { charge = "0%", cycles = 120, x = 0 }
    in
    times 10 (\_ -> p)


animateParticle : Int -> Int -> { index : Int, length : Int } -> Particle -> Particle
animateParticle start now indexLength particle =
    particle
        |> computeAnimated moveXAnimConfig start indexLength now
        |> computeAnimated chargeAnimConfig start indexLength now
        |> computeAnimated cyclesAnimConfig start indexLength now


moveXAnimConfig : AnimConfig Particle Float
moveXAnimConfig =
    { from = .obj >> .x
    , to = always 100
    , duration = always 1800
    , delay = .index >> mul 100 >> always 5000
    , setter = \v o -> { o | x = v }
    , interpolator = lerp
    , direction = always Normal
    , loop = always <| Infinite
    }


chargeAnimConfig : AnimConfig Particle String
chargeAnimConfig =
    { from = .obj >> .charge
    , to = always "100%"
    , duration = always 1800
    , delay = always 0
    , setter = \v o -> { o | charge = v }
    , interpolator = lerpPctString
    , direction = always Alternate
    , loop = always <| Times 3
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


computeAnimated : AnimConfig o v -> Int -> { index : Int, length : Int } -> Int -> o -> o
computeAnimated config start { index, length } now obj =
    let
        args : Args o
        args =
            Args obj index length

        duration =
            config.duration args

        delay =
            config.delay args

        loop =
            config.loop args

        fr =
            toFloat (now - (start - delay)) / toFloat duration

        maxIterations =
            case loop of
                Infinite ->
                    maxInt

                Times times ->
                    times - 1

        frac =
            (fr - toFloat (min maxIterations (floor fr)))
                |> clamp 0 1

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
                    config.interpolator from
                        to
                        (if isEven (min maxIterations (floor fr)) then
                            frac

                         else
                            1 - frac
                        )
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
    div
        [ style "transform" ("translateX(" ++ fromFloat p.x ++ "px)")
        , bgc <| hsl 0.2 1 0.5
        , fg transparent
        , styleWidth "50px"
        , styleHeight "50px"
        , ma "10px"
        ]
        [ div [] [ text p.charge ]
        , div [] [ text <| fromInt p.cycles ]
        ]
