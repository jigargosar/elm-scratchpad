module TIS100.Effect exposing (..)

import Utils exposing (pairTo)


type Effect
    = Focus String
    | None


withoutEffect =
    pairTo None
