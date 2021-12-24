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


type Direction
    = DirectionNormal
    | DirectionReverse
    | DirectionAlternate


type Loop
    = LoopForever
    | LoopFor Int


type alias AnimAttr =
    Anim -> Anim


anim : List AnimAttr -> Anim
anim fns =
    { from = 0
    , to = 1
    , start = 0
    , duration = 1800
    , delay = 0
    , direction = DirectionNormal
    , loop = LoopFor 1
    }
        |> applyAll fns


fromTo : Float -> Float -> AnimAttr
fromTo from to a =
    { a | from = from, to = to }


setDelay : Int -> AnimAttr
setDelay delay a =
    { a | delay = delay }


setDuration : Int -> AnimAttr
setDuration duration a =
    { a | duration = duration }


loopForever : AnimAttr
loopForever a =
    { a | loop = LoopForever }


alternateDirection : AnimAttr
alternateDirection a =
    { a | direction = DirectionAlternate }


valueAt : Anim -> Int -> Float
valueAt { from, to, start, duration, delay, direction, loop } now =
    if now >= start + delay then
        let
            fr =
                toFloat (now - (start + delay)) / toFloat duration

            maxIterations =
                case loop of
                    LoopForever ->
                        maxInt

                    LoopFor times ->
                        times - 1

            frac =
                (fr - toFloat (min maxIterations (floor fr)))
                    |> clamp 0 1

            value =
                case direction of
                    DirectionNormal ->
                        lerp from to frac

                    DirectionReverse ->
                        lerp from to (1 - frac)

                    DirectionAlternate ->
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


type alias Particle =
    { x : Float
    , xa : Anim
    }


initialParticles : List Particle
initialParticles =
    times 10
        (\i ->
            { x = 0
            , xa =
                anim
                    [ setDelay <| i * 500
                    , fromTo 0 270
                    , setDuration 1800
                    , loopForever
                    , alternateDirection
                    ]
            }
        )


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
