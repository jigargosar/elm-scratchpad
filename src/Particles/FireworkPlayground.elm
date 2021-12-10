module Particles.FireworkPlayground exposing (..)

import Playground exposing (..)


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

        minV =
            min s.width s.height

        oneHundredth =
            minV / 100

        pct n =
            n * oneHundredth

        lineLen =
            (s.width / 4) |> round |> toFloat
    in
    [ rectangle black s.width s.height
    , square black minV

    --, circle green (pct 1)
    , square white (lineLen * 2)
    , let
        r =
            pct 0.5

        samples =
            lineLen
                |> always (lineLen / (r * 2))
                |> round
      in
      List.range 0 samples
        |> List.map
            (\i ->
                circle green r
                    |> moveRight (toFloat i * (lineLen / toFloat samples))
                    |> fade 0.5
            )
        |> group

    --|> moveLeft (minV / 2)
    ]
