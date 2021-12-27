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


shouldReverse iteration direction =
    case direction of
        DirectionNormal ->
            False

        DirectionReverse ->
            True

        DirectionAlternate ->
            isEven iteration


type Loop
    = LoopForever
    | LoopFor Int


type alias AnimAttr =
    Anim -> Anim


initAnim : List AnimAttr -> Anim
initAnim fns =
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
    { a | loop = LoopFor (times |> atLeast 1) }



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
    | Ended { iteration : Int }


getStage : Anim -> AnimClock -> AnimStage
getStage a c =
    let
        elapsed =
            animClockElapsed c - a.delay
    in
    if elapsed <= 0 then
        NotStarted

    else
        let
            frac =
                toFloat (modBy a.duration elapsed) / toFloat a.duration

            iterationCount =
                (elapsed // a.duration) + 1
        in
        case a.loop of
            LoopForever ->
                Running { frac = frac, iteration = iterationCount }

            LoopFor maxIterations ->
                if maxIterations <= 0 then
                    NotStarted

                else if iterationCount <= maxIterations then
                    Running { frac = frac, iteration = iterationCount }

                else
                    Ended { iteration = maxIterations }


value : List AnimAttr -> AnimClock -> Float
value attrs c =
    let
        a =
            initAnim attrs
    in
    case getStage a c of
        NotStarted ->
            a.from

        Ended { iteration } ->
            if shouldReverse iteration a.direction then
                a.from

            else
                a.to

        Running { frac, iteration } ->
            frac
                |> applyDirectionToFrac iteration a.direction
                |> a.easing
                |> lerp a.from a.to


applyDirectionToFrac : Int -> Direction -> Float -> Float
applyDirectionToFrac iteration direction frac =
    if shouldReverse iteration direction then
        1 - frac

    else
        frac


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
