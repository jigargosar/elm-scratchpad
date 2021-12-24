module Anime.AnimeClone_V2 exposing (main)

import Browser.Events
import Ease
import Json.Decode as JD
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { animClock : Int
    , particles : List Particle
    }


particlesForRendering : Model -> List Particle
particlesForRendering model =
    model.particles
        |> List.map (updateParticleAnim model.animClock)


initParticle : IndexLength -> Particle
initParticle { index, length } =
    let
        defaultAttrs =
            [ setDuration 1800

            --, loopForever
            , alternateDirection
            , setEasing Ease.outBack
            , setEasing Ease.linear
            ]
    in
    { x = 0
    , xa =
        anim
            (defaultAttrs
                ++ [ fromTo 0 270

                   --, setDelay <| i * 500
                   ]
            )
    , a = 0
    , aa = anim (defaultAttrs ++ [ fromTo -360 360 ])
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        len =
            10

        initialParticles : List Particle
        initialParticles =
            timesWithIndexAndLength len initParticle
    in
    ( { animClock = 0
      , particles = initialParticles
      }
    , Cmd.none
    )


type Msg
    = NOP
    | OnAnimationFrameDeltaMilli Int
    | OnClick


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Browser.Events.onAnimationFrameDelta (round >> OnAnimationFrameDeltaMilli)
    , Browser.Events.onClick (JD.succeed OnClick)
    ]
        |> Sub.batch


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

        OnClick ->
            ( { model | animClock = 0 }, Cmd.none )


view : Model -> Document Msg
view model =
    Document "Anime V2"
        [ basicStylesNode
        , div []
            (particlesForRendering model
                |> List.map viewParticle
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
    , easing : Ease.Easing
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
    , easing = Ease.linear
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


setEasing : Ease.Easing -> AnimAttr
setEasing easing a =
    { a | easing = easing }


loopForever : AnimAttr
loopForever a =
    { a | loop = LoopForever }


alternateDirection : AnimAttr
alternateDirection a =
    { a | direction = DirectionAlternate }


valueAt : Anim -> Int -> Float
valueAt { from, to, start, duration, delay, direction, loop, easing } now =
    if now < start + delay then
        from

    else if now > start + delay + duration then
        to

    else
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
                fr - toFloat (min maxIterations (floor fr))

            --|> clamp 0 1
            value =
                case direction of
                    DirectionNormal ->
                        lerp from to (easing frac)

                    DirectionReverse ->
                        lerp from to (Ease.reverse easing frac)

                    DirectionAlternate ->
                        lerp from
                            to
                            (if isEven (min maxIterations (floor fr)) then
                                easing frac

                             else
                                Ease.reverse easing frac
                            )
        in
        value


type alias Particle =
    { x : Float
    , xa : Anim
    , a : Float
    , aa : Anim
    }


updateParticleAnim : Int -> Particle -> Particle
updateParticleAnim now p =
    { p
        | x = valueAt p.xa now
        , a = valueAt p.aa now
    }


viewParticle : Particle -> Html msg
viewParticle p =
    div
        [ style "transform"
            ([ "translateX(" ++ fromFloat p.x ++ "px)"
             , "rotate(" ++ fromFloat p.a ++ "deg)"
             ]
                |> String.join " "
            )
        , bgc <| hsl 0.2 1 0.5
        , fg black
        , borderRadius "10px"
        , styleWidth "50px"
        , styleHeight "50px"
        , ma "10px"
        , dGrid
        , placeContentCenter
        ]
        [ div [ fontSize "25px" ] [ text "A" ] ]
