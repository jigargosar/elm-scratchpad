module TIS100.IOIntent exposing (..)

import TIS100.Num exposing (Num)
import Utils exposing (Dir4)


type IOIntent
    = Read Dir4
    | Write Dir4 (Maybe Num)
