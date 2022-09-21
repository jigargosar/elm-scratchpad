module TIS100.PuzzlePage.Inst exposing (..)

import TIS100.Num exposing (Num)
import Utils exposing (Dir4)


type Inst
    = Mov Src Dst
    | Nop
    | Jmp String
    | Jez String
    | Jnz String
    | Jgz String
    | Jlz String
    | Jro Src


type Src
    = SrcPort Dir4
    | SrcNum Num
    | SrcAcc


type Dst
    = DstPort Dir4
    | DstAcc
    | DstNil


getSrcAndDstOperands : Inst -> ( Maybe Src, Maybe Dst )
getSrcAndDstOperands inst =
    case inst of
        Mov src dst ->
            ( Just src, Just dst )

        Jro src ->
            ( Just src, Nothing )

        Nop ->
            ( Nothing, Nothing )

        Jmp _ ->
            ( Nothing, Nothing )

        Jnz _ ->
            ( Nothing, Nothing )

        Jez _ ->
            ( Nothing, Nothing )

        Jgz _ ->
            ( Nothing, Nothing )

        Jlz _ ->
            ( Nothing, Nothing )
