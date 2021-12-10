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
    in
    [ rectangle black s.width s.height
    , square black minV

    --, circle green (pct 1)
    , let
        len =
            s.width / 4

        r =
            pct 5

        foo =
            100

        samples =
            (len / (r * 2)) * foo
      in
      List.range 0 (round samples)
        |> List.map
            (toFloat
                >> (\i ->
                        circle green r
                            |> moveRight (i * (len / samples))
                            |> fade 0.5
                   )
            )
        |> group

    --|> moveLeft (minV / 2)
    ]
