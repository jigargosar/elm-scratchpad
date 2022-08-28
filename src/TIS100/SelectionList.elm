module TIS100.SelectionList exposing (..)

import Pivot exposing (Pivot)


type SelectionList a
    = None (List a)
    | Selected (Pivot a)
