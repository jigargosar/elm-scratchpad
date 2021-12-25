module Anime.Anim exposing
    ( Anim
    , AnimAttr
    , AnimClock
    , AnimClockDelta
    , alternateDirection
    , anim
    , animClockInit
    , animClockSubscription
    , animClockUpdateOnDelta
    , fromTo
    , loopForever
    , loopTimes
    , reverseDirection
    , setDelay
    , setDuration
    , setEasing
    , setTo
    , stagger
    , staggerFromCenter
    , staggerRange
    , valueAt
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


fromTo : Float -> Float -> AnimAttr
fromTo from to a =
    { a | from = from, to = to }


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



--noinspection ElmUnusedSymbol


loopForever : AnimAttr
loopForever a =
    { a | loop = LoopForever }


loopTimes : Int -> AnimAttr
loopTimes times a =
    { a | loop = LoopFor (times |> atLeast 0) }



--noinspection ElmUnusedSymbol


alternateDirection : AnimAttr
alternateDirection a =
    { a | direction = DirectionAlternate }


reverseDirection : AnimAttr
reverseDirection a =
    { a | direction = DirectionReverse }


type AnimStage
    = NotStarted
    | Running
    | Ended Int


type Iteration
    = IterationEven
    | IterationOdd


getStage : Anim -> AnimClock -> AnimStage
getStage { duration, delay, direction, loop } { start, current } =
    let
        elapsed =
            current - start
    in
    if elapsed < delay then
        NotStarted

    else
        case loop of
            LoopForever ->
                Running

            LoopFor times ->
                if elapsed >= delay + (times * duration) then
                    Ended times

                else
                    Running


valueAt : Anim -> AnimClock -> Float
valueAt a c =
    case getStage a c of
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

        Running ->
            valueAtWhenRunning a c


valueAtWhenRunning : Anim -> AnimClock -> Float
valueAtWhenRunning { from, to, duration, delay, direction, loop, easing } ac =
    let
        elapsed =
            (ac.current - (ac.start + delay))
                -- need to ensure elapsed is positive, in case of delay
                |> atLeast 0

        iterationCount =
            (elapsed // duration) + 1

        frac =
            (toFloat (modBy duration elapsed) / toFloat duration)
                |> clamp 0 1

        applyDirection =
            case direction of
                DirectionNormal ->
                    identity

                DirectionReverse ->
                    Ease.reverse

                DirectionAlternate ->
                    if isOdd iterationCount then
                        identity

                    else
                        Ease.reverse
    in
    lerp from to (applyDirection easing frac)


staggerRange : Float2 -> IndexLength -> Float
staggerRange ( from, to ) il =
    let
        frac =
            toFloat il.index / (toFloat il.length - 1)
    in
    lerp from to frac



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
