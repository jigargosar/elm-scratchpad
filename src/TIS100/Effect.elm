module TIS100.Effect exposing (..)

import Html.Attributes as HA
import TIS100.Addr exposing (Addr)
import TIS100.Puzzle as Puzzle
import Utils exposing (pairTo)


type Effect
    = AutoFocus
    | ReturnToSegmentList
    | SavePuzzleSrc Puzzle.Id (List ( Addr, String ))
    | None


save =
    SavePuzzleSrc


none =
    None


returnToSegmentList =
    ReturnToSegmentList


withoutEff =
    pairTo None


withEff eff =
    pairTo eff


withEffBy fn m =
    ( m, fn m )


autoFocus =
    AutoFocus


autoFocusId =
    "auto-focus"


attrAutoFocusId =
    HA.id autoFocusId
