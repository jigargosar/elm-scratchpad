module TIS100.Stream exposing (..)

import Pivot exposing (Pivot)
import Utils as U exposing (LCR)


type Stream a
    = Ended (List a)
    | Stream (Pivot a)


fromList : List a -> Stream a
fromList ls =
    case Pivot.fromList ls of
        Just p ->
            Stream p

        Nothing ->
            Ended []
