module TIS100Modeling exposing (..)

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

type Inst1 = Add | Sub | JRO | Set

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
    NOPAfterRead | WriteAfterRead DstPort

type Result e v = Ok v | Err e
type Maybe a = Just a | Nothing

type alias SrcPortResolver = (SrcPort -> Maybe (Num, Maybe LastAnyPort))


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
    case nec.state of
        Run ->
            case currentInst nec.exe nec.pc of
                Inst1 instRS src ->
                    let
                        updateWithNum n =
                            update (MsgWithNum instRS n) nec
                    in
                    case srcToNum nec src of
                        Ok n -> updateWithNum n
                        Err sp ->
                            case srcPortResolver sp of
                                Just (n, maybeLast) ->
                                    updateWithNum n |> updateLastAnyPort maybeLast
                                Nothing ->
                                    -- mark current cycle idle.
                                    {nec| state = Read1 instRS sp}
                Mov src dst ->
                    case (src,dst) of
                        (SrcPort sp, DstNil) -> {nec| state = Read NOPAfterRead sp}
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


        Read1 instRS sp ->
            let
                updateWithNum n = update (MsgWithNum instRS n) nec
            in
            case srcPortResolver sp of
                Just (n, maybeLast) -> updateWithNum n |> updateLastAnyPort maybeLast
                Nothing ->
                    -- mark current cycle idle.
                   {nec| state = Read1 instRS sp}

        Read afterReadMsg sp ->
            case srcPortResolver sp of
                Just (n, maybeLast) ->
                    let
                        newNec =
                            case afterReadMsg of
                                NOPAfterRead -> update NOP nec
                                WriteAfterRead dp -> {nec| state = Write dp n}
                    in
                        updateLastAnyPort maybeLast newNec
                Nothing -> nec -- mark this cycle idle.



        Write _ _ ->
            -- on write success, i.e. opp port is in Read from op state
            update NOP nec



