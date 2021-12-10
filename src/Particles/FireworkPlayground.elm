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
    , placeShapeOnLine
        50
        (circle white 5 |> fade 1.2)
        (U.vec 0 0)
        (U.vec 200 200)
        |> fade 1
    , let
        ( st, e ) =
            ( U.vec 0 0, U.vec -200 -200 )

        vd =
            U.vFromTo st e
      in
      group
        (U.normSamples 50
            |> List.map
                (\t ->
                    circle yellow 3
                        |> shMV (U.vScale t vd |> U.vAdd st)
                )
        )
    ]


placeShapeOnLine : Int -> Shape -> U.Vec -> U.Vec -> Shape
placeShapeOnLine sampleCount sh s e =
    group
        (U.sampleVecFromTo sampleCount s e
            |> List.map (mvSh sh)
        )


mvSh : Shape -> U.Vec -> Shape
mvSh sh { x, y } =
    sh |> move x y


shMV a b =
    mvSh b a
