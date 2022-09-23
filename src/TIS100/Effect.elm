module TIS100.Effect exposing (..)

import Html.Attributes as HA
import Utils exposing (pair, pairTo, swap)


type Effect
    = Focus String
    | None


withoutEff =
    pairTo None


withEff eff =
    pairTo eff


autoFocus =
    Focus autoFocusId


autoFocusId =
    "auto-focus"


attrAutoFocusId =
    HA.id autoFocusId
