module TIS100.PuzzlePage.Inst exposing (..)

import TIS100.Num exposing (Num)
import Utils exposing (Dir4)


type Inst
    = Mov Src Dst
    | Nop


type Src
    = SrcPort Dir4
    | SrcNum Num
    | SrcAcc


type Dst
    = DstPort Dir4
    | DstAcc
    | DstNil