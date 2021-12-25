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
    { animClock : AnimClock
    , particles : List Particle
    }


type alias AnimClock =
    { start : Int
    , current : Int
    }


animClockInit : AnimClock
animClockInit =
    AnimClock 0 0


animClockUpdateOnDelta : AnimClockDelta -> AnimClock -> AnimClock
animClockUpdateOnDelta (AnimClockDelta deltaMilli) ac =
    { ac | current = ac.current + clamp 0 100 deltaMilli }


type AnimClockDelta
    = AnimClockDelta Int


animClockSubscription : (AnimClockDelta -> msg) -> Sub msg
animClockSubscription tag =
    Browser.Events.onAnimationFrameDelta (round >> AnimClockDelta >> tag)


particlesForRendering : Model -> List Particle
particlesForRendering model =
    model.particles
        |> List.map (updateParticleAnim model.animClock)


initParticle : IndexLength -> Particle
initParticle il =
    let
        defaultAttrs =
            [ setDuration 1800
            , loopForever
            , reverseDirection
            , setEasing Ease.linear
            , setDelay <| round <| staggerFromCenter 500 il
            ]
    in
    { x = 0
    , xa =
        anim
            ([ defaultAttrs, [ fromTo 0 270 ] ] |> List.concat)
    , a = 0
    , aa =
        anim
            (defaultAttrs
                ++ [ setTo <| staggerRange ( -360, 360 ) il
                   , setTo 0
                   ]
            )
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialParticles : List Particle
        initialParticles =
            timesWithIndexAndLength 6 initParticle
    in
    ( { animClock = animClockInit
      , particles = initialParticles
      }
    , Cmd.none
    )


type Msg
    = NOP
    | OnAnimClockDelta AnimClockDelta
    | OnClick


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ animClockSubscription OnAnimClockDelta
    , Browser.Events.onClick (JD.succeed OnClick)
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnAnimClockDelta delta ->
            ( { model
                | animClock = animClockUpdateOnDelta delta model.animClock
              }
            , Cmd.none
            )

        OnClick ->
            ( { model | animClock = animClockInit }, Cmd.none )


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


staggerRange : Float2 -> IndexLength -> Float
staggerRange ( from, to ) il =
    let
        frac =
            toFloat il.index / (toFloat il.length - 1)
    in
    lerp from to frac


stagger : Float -> IndexLength -> Float
stagger offset il =
    offset * toFloat il.index


staggerFromCenter : Float -> IndexLength -> Float
staggerFromCenter offset il =
    let
        frac =
            (toFloat il.index - (toFloat (il.length - 1) / 2))
                |> abs
                |> round
                |> toFloat
                |> mul offset
    in
    frac


setTo : Float -> AnimAttr
setTo to a =
    { a | to = to }


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


loopTimes : Int -> AnimAttr
loopTimes times a =
    { a | loop = LoopFor times }


alternateDirection : AnimAttr
alternateDirection a =
    { a | direction = DirectionAlternate }


reverseDirection : AnimAttr
reverseDirection a =
    { a | direction = DirectionReverse }


valueAt : Anim -> AnimClock -> Float
valueAt { from, to, duration, delay, direction, loop, easing } ac =
    let
        now =
            ac.current

        start =
            ac.start
    in
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


updateParticleAnim : AnimClock -> Particle -> Particle
updateParticleAnim ac p =
    { p
        | x = valueAt p.xa ac
        , a = valueAt p.aa ac
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
