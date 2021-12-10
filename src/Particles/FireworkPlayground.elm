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
    , strokeLine (circle white 10) -200 -200 200 200 |> fade 1
    ]


strokeLine sh a b c d =
    let
        ( s, e ) =
            ( U.vec a b, U.vec c d )

        viewPt { x, y } =
            sh
                |> move x y
                |> fade 0.1

        pts =
            U.sampleVecFromTo 100 s e
                |> List.map viewPt
    in
    group pts
