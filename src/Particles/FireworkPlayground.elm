module Particles.FireworkPlayground exposing (..)

import Playground exposing (..)
import Utils as U


main =
    game view update initial


type alias Model =
    {}


initial =
    Model


update : Computer -> Model -> Model
update c m =
    m


view : Computer -> Model -> List Shape
view c m =
    let
        s : Screen
        s =
            c.screen
    in
    [ rectangle black s.width s.height
    , square white 400 |> fade 0.1
    , strokeLine white 20 0 0 0 200 |> fade 0.9
    ]


strokeLine co th a b c d =
    let
        ( s, e ) =
            ( U.vec a b, U.vec c d )

        sh =
            circle co (th / 2)

        viewPt { x, y } =
            sh |> move x y

        sampleCount =
            10

        pts =
            List.range 1 sampleCount
                |> List.map (toFloat >> U.norm 1 sampleCount)
                |> List.map (\i -> U.vScale i (U.vFromTo s e))
                |> List.map viewPt
    in
    group
        ((\_ ->
            [ viewPt s, viewPt e ]
         )
            |> always pts
        )
