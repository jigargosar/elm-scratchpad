module Anime.Anim exposing
    ( Anim
    , AnimAttr
    , AnimClock
    , AnimClockDelta
    , alternateDirection
    , animClockInit
    , animClockSubscription
    , animClockUpdateOnDelta
    , delay
    , duration
    , ease
    , fromTo
    , loopFor
    , loopForever
    , reverseDirection
    , stagger
    , staggerFromCenter
    , staggerRange
    , to
    , value
    )

import Browser.Events
import Ease
import Utils exposing (..)


type alias AnimClock =
    { start : Int
    , current : Int
    }


animClockInit : AnimClock
animClockInit =
    AnimClock 0 0


animClockElapsed : AnimClock -> Int
animClockElapsed c =
    c.current - c.start


animClockUpdateOnDelta : AnimClockDelta -> AnimClock -> AnimClock
animClockUpdateOnDelta (AnimClockDelta deltaMilli) ac =
    { ac | current = ac.current + clamp 0 100 deltaMilli }


type AnimClockDelta
    = AnimClockDelta Int


animClockSubscription : (AnimClockDelta -> msg) -> Sub msg
animClockSubscription tag =
    Browser.Events.onAnimationFrameDelta (round >> AnimClockDelta >> tag)


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



--noinspection ElmUnusedSymbol


fromTo : Float -> Float -> AnimAttr
fromTo from to_ a =
    { a | from = from, to = to_ }



--noinspection ElmUnusedSymbol


to : Float -> AnimAttr
to to_ a =
    { a | to = to_ }


delay : Int -> AnimAttr
delay delay_ a =
    { a | delay = delay_ }


duration : Int -> AnimAttr
duration duration_ a =
    { a | duration = duration_ }


ease : Ease.Easing -> AnimAttr
ease easing a =
    { a | easing = easing }



--noinspection ElmUnusedSymbol


loopForever : AnimAttr
loopForever a =
    { a | loop = LoopForever }



--noinspection ElmUnusedSymbol


loopFor : Int -> AnimAttr
loopFor times a =
    { a | loop = LoopFor (times |> atLeast 0) }



--noinspection ElmUnusedSymbol


alternateDirection : AnimAttr
alternateDirection a =
    { a | direction = DirectionAlternate }



--noinspection ElmUnusedSymbol


reverseDirection : AnimAttr
reverseDirection a =
    { a | direction = DirectionReverse }


type AnimStage
    = NotStarted
    | Running { frac : Float, iteration : Int }
    | Ended Int


getStage : Anim -> Int -> AnimStage
getStage a clock =
    let
        elapsed =
            clock - a.delay
    in
    if elapsed <= 0 then
        NotStarted

    else
        let
            iterationCount =
                (elapsed // a.duration) + 1
        in
        let
            frac =
                toFloat (modBy a.duration elapsed) / toFloat a.duration
        in
        case a.loop of
            LoopForever ->
                Running { frac = frac, iteration = iterationCount }

            LoopFor times ->
                if elapsed >= a.delay + (times * a.duration) then
                    Ended times

                else
                    Running { frac = frac, iteration = iterationCount }


value : List AnimAttr -> AnimClock -> Float
value attrs =
    valueAtHelp (anim attrs)


valueAtHelp : Anim -> AnimClock -> Float
valueAtHelp a c =
    case getStage a (animClockElapsed c) of
        NotStarted ->
            a.from

        Ended iterations ->
            case a.direction of
                DirectionNormal ->
                    a.to

                DirectionReverse ->
                    a.from

                DirectionAlternate ->
                    if isOdd iterations then
                        a.to

                    else
                        a.from

        Running _ ->
            valueAtWhenRunning a c


valueAtWhenRunning : Anim -> AnimClock -> Float
valueAtWhenRunning a ac =
    let
        elapsed =
            (ac.current - ac.start - a.delay)
                -- need to ensure elapsed is positive, in case of delay
                |> atLeast 0
    in
    let
        iterationCount =
            (elapsed // a.duration) + 1

        shouldReverse =
            case a.direction of
                DirectionNormal ->
                    False

                DirectionReverse ->
                    True

                DirectionAlternate ->
                    isEven iterationCount

        applyDirection : Float -> Float
        applyDirection n =
            if shouldReverse then
                1 - n

            else
                n
    in
    let
        frac =
            (toFloat (modBy a.duration elapsed) / toFloat a.duration)
                |> applyDirection
                |> a.easing
    in
    lerp a.from a.to frac


staggerRange : Float2 -> IndexLength -> Float
staggerRange ( from, to_ ) il =
    let
        frac =
            toFloat il.index / (toFloat il.length - 1)
    in
    lerp from to_ frac



--noinspection ElmUnusedSymbol


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
