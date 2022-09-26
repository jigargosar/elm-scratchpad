module TIS100.Effect exposing (..)

import Html.Attributes as HA
import TIS100.Addr exposing (Addr)
import Utils exposing (pairTo)


type Effect
    = AutoFocus
    | ReturnToSegmentList
    | SavePuzzleSrc (List ( Addr, String ))
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
