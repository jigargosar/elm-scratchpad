module TIS100.Effect exposing (..)

import Html.Attributes as HA
import Utils exposing (pairTo)


type Effect
    = AutoFocus
    | ReturnToSegmentList
    | None


none =
    None


returnToSegmentList =
    ReturnToSegmentList


withoutEff =
    pairTo None


withEff eff =
    pairTo eff


autoFocus =
    AutoFocus


autoFocusId =
    "auto-focus"


attrAutoFocusId =
    HA.id autoFocusId
