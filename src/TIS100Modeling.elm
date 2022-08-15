module TIS100Modeling exposing (..)

import Utils exposing (pairTo)
type Num = Num
type PC = PC
type EXE = EXE
type SrcPort = InPortDir PortDir | InPortAny
type DstPort = OutPortDir PortDir | OutPortAny
type PortDir = L|R|U|D

type LastAnyPort = LastAnyPort PortDir | LastAnyPortNone
zero : Num
zero = Num

type alias NEC =
    { exe : EXE
    , idle: Int
    --, sc : SC
    , pc: PC
    , acc : Num
    , bak : Num
    , last : LastAnyPort
    , state: NECState
    }

type NECState
    = Run
    | Write DstPort Num
    | Read AfterReadMsg SrcPort
    | Read1 Inst1 SrcPort

type Inst1 = Add | Sub | JRO | Set | NOP1

type Inst = Mov Src Dst | Inst1 Inst1 Src
type Src = SrcNil | SrcNum Num | SrcAcc | SrcPort SrcPort
type Dst = DstNil | DstAcc | DstPort DstPort


type Msg =  NOP | MsgWithNum Inst1 Num


update: Msg -> {a| pc:PC,acc:Num,bak: Num} -> {a| pc:PC,acc:Num,bak: Num}
update _ a = a

currentInst: EXE -> PC -> Inst
currentInst _ _ =
    Inst1 Add SrcAcc

type AfterReadMsg =
    WriteAfterRead DstPort


type alias SrcPortResolver = SrcPort -> Maybe (Num, Maybe LastAnyPort)


srcToNum: {a| acc: Num} -> Src -> Result SrcPort Num
srcToNum {acc} src =
    case src of
        SrcNil -> Ok zero
        SrcNum n -> Ok n
        SrcAcc -> Ok acc
        SrcPort sp -> Err sp

updateLastAnyPort: Maybe LastAnyPort -> {a| last: LastAnyPort} -> {a| last: LastAnyPort}
updateLastAnyPort maybeLast model =
    case maybeLast of
        Just last -> {model | last = last}
        Nothing -> model

step: SrcPortResolver -> NEC -> NEC
step srcPortResolver nec =
    let
        resolveSrc : Src -> Result SrcPort (Num, Maybe LastAnyPort)
        resolveSrc src =
            case srcToNum nec src of
                Ok n -> Ok (n, Nothing)
                Err sp -> srcPortResolver sp |> Result.fromMaybe sp
    in
    case nec.state of
        Run ->
            case currentInst nec.exe nec.pc of
                Inst1 inst1 src ->
                    case resolveSrc src of
                        Ok (n, maybeLast) ->
                            update (MsgWithNum inst1 n) nec |> updateLastAnyPort maybeLast
                        Err sp -> { nec | state = Read1 inst1 sp, idle = nec.idle + 1 }
                Mov src dst ->
                    case (src,dst) of
                        (SrcPort sp, DstNil) -> {nec| state = Read1 NOP1 sp}
                        (SrcPort sp, DstAcc) -> {nec| state = Read1 Set sp}
                        (SrcPort sp, DstPort dp) -> {nec| state = Read (WriteAfterRead dp) sp}
                        --
                        (SrcNil, DstPort dp) -> {nec| state = Write dp zero}
                        (SrcNum n, DstPort dp) -> {nec| state = Write dp n}
                        (SrcAcc, DstPort dp) -> {nec| state = Write dp nec.acc}
                        --
                        (SrcNil, DstNil) -> update NOP nec
                        (SrcNum _, DstNil) -> update NOP nec
                        (SrcAcc, DstNil) -> update NOP nec
                        --
                        (SrcNil, DstAcc) -> update (MsgWithNum Set zero) nec
                        (SrcNum n, DstAcc) -> update (MsgWithNum Set n) nec
                        (SrcAcc, DstAcc) -> update NOP nec


        Read1 inst1 sp ->
            case srcPortResolver sp of
                Just (n, maybeLast) ->
                    update (MsgWithNum inst1 n) nec |> updateLastAnyPort maybeLast
                Nothing -> {nec| state = Read1 inst1 sp, idle = nec.idle + 1}

        Read afterReadMsg sp ->
            case srcPortResolver sp of
                Just (n, maybeLast) ->
                    let
                        newNec =
                            case afterReadMsg of
                                WriteAfterRead dp -> {nec| state = Write dp n}
                    in
                        updateLastAnyPort maybeLast newNec
                Nothing -> nec -- mark this cycle idle.



        Write _ _ ->
            -- on write success, i.e. opp port is in Read from op state
            update NOP nec



