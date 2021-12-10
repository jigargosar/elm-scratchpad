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
    , square black (pct 50)
    , square white (lineLen * 2)
    ]
