module TIS100.IOIntent exposing (..)

import Utils exposing (Dir4)


type IOIntent
    = Read Dir4
    | Write Dir4
