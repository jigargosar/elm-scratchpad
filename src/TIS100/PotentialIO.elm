module TIS100.PotentialIO exposing (..)

import Utils exposing (Dir4)


type PotentialIO
    = MayRead Dir4
    | MayWrite Dir4
