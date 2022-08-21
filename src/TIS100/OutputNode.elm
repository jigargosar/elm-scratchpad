module TIS100.OutputNode exposing
    ( OutputNode
    , fromExpected
    , step
    )

import TIS100.Num exposing (Num)


type OutputNode
    = Done (List Num)
    | Running Int (List Num)
    | ReadBlocked Int (List Num)


fromExpected : Int -> OutputNode
fromExpected expected =
    if expected <= 0 then
        Done []

    else
        Running expected []


type alias ReadFn a =
    () -> Maybe ( Num, a )


step : ReadFn a -> OutputNode -> ( OutputNode, Maybe a )
step readFn node =
    let
        attemptRead pendingReads nums =
            case readFn () of
                Nothing ->
                    ( ReadBlocked pendingReads nums, Nothing )

                Just ( num, a ) ->
                    let
                        fn =
                            if pendingReads == 1 then
                                Done

                            else
                                Running (pendingReads - 1)
                    in
                    ( fn (num :: nums), Just a )
    in
    case node of
        Done _ ->
            ( node, Nothing )

        Running pendingReads nums ->
            attemptRead pendingReads nums

        ReadBlocked pendingReads nums ->
            attemptRead pendingReads nums
