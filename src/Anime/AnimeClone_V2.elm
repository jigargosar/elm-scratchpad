module Anime.AnimeClone_V2 exposing (main)

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
    Document "Anime V2"
        [ basicStylesNode
        , div []
            (initialParticles
                |> List.map (updateParticleAnim model.animClock >> viewParticle)
            )
        ]


type alias Anim =
    { from : Float
    , to : Float
    , start : Int
    , duration : Int
    , delay : Int
    , direction : Direction
    , loop : Loop
    }


initAnim : Int -> Anim
initAnim start =
    { from = 0
    , to = 1
    , start = start
    , duration = 1800
    , delay = 0
    , direction = Normal
    , loop = Times 1
    }


valueAt : Anim -> Int -> Float
valueAt { from, to, start, duration, delay, direction, loop } now =
    if now >= start + delay then
        let
            fr =
                toFloat (now - (start + delay)) / toFloat duration

            maxIterations =
                case loop of
                    Infinite ->
                        maxInt

                    Times times ->
                        times - 1

            frac =
                (fr - toFloat (min maxIterations (floor fr)))
                    |> clamp 0 1

            value =
                case direction of
                    Normal ->
                        lerp from to frac

                    Reverse ->
                        lerp from to (1 - frac)

                    Alternate ->
                        lerp from
                            to
                            (if isEven (min maxIterations (floor fr)) then
                                frac

                             else
                                1 - frac
                            )
        in
        value

    else
        from


type Direction
    = Normal
    | Reverse
    | Alternate


type Loop
    = Infinite
    | Times Int


type alias Particle =
    { x : Float
    , xa : Anim
    }


initialParticles : List Particle
initialParticles =
    let
        xa =
            initAnim 0
    in
    times 10 (\_ -> { x = 0, xa = { xa | from = 0, to = 100 } })


updateParticleAnim : Int -> Particle -> Particle
updateParticleAnim now p =
    { p | x = valueAt p.xa now }


viewParticle : Particle -> Html msg
viewParticle p =
    div
        [ style "transform" ("translateX(" ++ fromFloat p.x ++ "px)")
        , bgc <| hsl 0.2 1 0.5
        , borderRadius "10px"
        , styleWidth "50px"
        , styleHeight "50px"
        , ma "10px"
        ]
        []
