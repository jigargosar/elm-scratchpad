module TIS100.IOIntent exposing (..)

import Utils exposing (Dir4)


type IOIntent
    = MayRead Dir4
    | MayWrite Dir4
