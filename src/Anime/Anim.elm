module Anime.Anim exposing
    ( Anim
    , AnimAttr
    , Clock
    , ClockMsg
    , alternateDirection
    , clockFromElapsedMillis
    , clockSubscription
    , delay
    , duration
    , ease
    , from
    , fromTo
    , fromToF2
    , initClock
    , loopFor
    , loopForever
    , mapListFor2Values
    , mapListForOneValue
    , reverseDirection
    , stagger
    , staggerDelay
    , staggerEase
    , staggerFromCenter
    , staggerRange
    , staggerReverse
    , staggerTo
    , to
    , updateClock
    , value
    )

import Browser.Events
import Ease
import Utils exposing (..)


type alias Clock =
    { start : Int
    , current : Int
    }


initClock : Clock
initClock =
    Clock 0 0


clockFromElapsedMillis : Int -> Clock
clockFromElapsedMillis =
    Clock 0


startClockAt : Int -> Clock
startClockAt ms =
    Clock ms ms


clockElapsed : Clock -> Int
clockElapsed c =
    c.current - c.start


updateClock : ClockMsg -> Clock -> Clock
updateClock (OnClockDelta deltaMilli) ac =
    { ac | current = ac.current + clamp 0 100 deltaMilli }


type ClockMsg
    = OnClockDelta Int


clockSubscription : (ClockMsg -> msg) -> Sub msg
clockSubscription tag =
    Browser.Events.onAnimationFrameDelta (round >> OnClockDelta >> tag)


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
    IndexLength -> Anim -> Anim



--noinspection ElmUnusedSymbol


fromTo : Float -> Float -> AnimAttr
fromTo from_ to_ _ a =
    { a | from = from_, to = to_ }


fromToF2 : Float2 -> AnimAttr
fromToF2 ( f, t ) =
    fromTo f t



--noinspection ElmUnusedSymbol


to : Float -> AnimAttr
to to_ _ a =
    { a | to = to_ }


from : Float -> AnimAttr
from from_ _ a =
    { a | from = from_ }


type alias StaggeredValue =
    IndexLength -> Float


staggerTo : StaggeredValue -> AnimAttr
staggerTo sv il a =
    { a | to = sv il }


delay : Int -> AnimAttr
delay delay_ _ a =
    { a | delay = delay_ }


staggerDelay : StaggeredValue -> AnimAttr
staggerDelay sv il a =
    { a | delay = sv il |> round }


duration : Int -> AnimAttr
duration duration_ _ a =
    { a | duration = duration_ }


ease : Ease.Easing -> AnimAttr
ease easing _ a =
    { a | easing = easing }



--noinspection ElmUnusedSymbol


loopForever : AnimAttr
loopForever _ a =
    { a | loop = LoopForever }



--noinspection ElmUnusedSymbol


loopFor : Int -> AnimAttr
loopFor times _ a =
    { a | loop = LoopFor (times |> atLeast 1) }



--noinspection ElmUnusedSymbol


alternateDirection : AnimAttr
alternateDirection _ a =
    { a | direction = DirectionAlternate }



--noinspection ElmUnusedSymbol


reverseDirection : AnimAttr
reverseDirection _ a =
    { a | direction = DirectionReverse }


type AnimStage
    = NotStarted
    | Running { frac : Float, iteration : Int }
    | Ended { iteration : Int }


stageAt : Anim -> Clock -> AnimStage
stageAt a c =
    let
        elapsed =
            clockElapsed c - a.delay
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


mapListFor2Values :
    List AnimAttr
    -> List AnimAttr
    -> (Float -> Float -> a -> b)
    -> Clock
    -> List a
    -> List b
mapListFor2Values attrs1 attrs2 fn clock =
    mapWithIndexAndLength
        (\il ->
            fn
                (valueWithIndexLength il attrs1 clock)
                (valueWithIndexLength il attrs2 clock)
        )


mapListForOneValue :
    List AnimAttr
    -> (Float -> a -> b)
    -> Clock
    -> List a
    -> List b
mapListForOneValue attrs1 fn clock =
    mapWithIndexAndLength
        (\il ->
            fn (valueWithIndexLength il attrs1 clock)
        )


valueWithIndexLength : IndexLength -> List AnimAttr -> Clock -> Float
valueWithIndexLength il attrs c =
    let
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
                |> applyAll (List.map ((|>) il) fns)
    in
    let
        a =
            initAnim attrs
    in
    case stageAt a c of
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


value : List AnimAttr -> Clock -> Float
value =
    valueWithIndexLength (IndexLength 0 1)


applyDirectionToFrac : Int -> Direction -> Float -> Float
applyDirectionToFrac iteration direction frac =
    if shouldReverse iteration direction then
        1 - frac

    else
        frac


staggerRange : Float2 -> IndexLength -> Float
staggerRange ( from_, to_ ) il =
    if il.length <= 1 then
        from_

    else
        let
            frac =
                toFloat il.index / (toFloat il.length - 1)
        in
        lerp from_ to_ frac



--noinspection ElmUnusedSymbol


stagger : Float -> IndexLength -> Float
stagger offset il =
    offset * toFloat il.index


staggerReverse : Float -> IndexLength -> Float
staggerReverse offset il =
    offset * toFloat ((il.length - 1) - il.index)


staggerEase : Float -> Ease.Easing -> IndexLength -> Float
staggerEase offset easing il =
    if il.length <= 1 then
        0

    else
        easing (toFloat il.index / toFloat (il.length - 1))
            |> mul (toFloat (il.length - 1) * offset)


staggerFromCenter : Float -> IndexLength -> Float
staggerFromCenter offset il =
    if il.length <= 1 then
        0

    else
        let
            frac =
                (toFloat il.index - (toFloat (il.length - 1) / 2))
                    |> abs
                    |> round
                    |> toFloat
                    |> mul offset
        in
        frac
