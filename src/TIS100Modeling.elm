module TIS100Modeling exposing (..)

import Utils exposing (pairTo)
type Num = Num
type PC = PC
type EXE = EXE
type SrcPort = InPortDir Dir | InPortAny
type DstPort = OutPortDir Dir | OutPortAny
type Dir = L|R|U|D

type LastAnyPort = LastAnyPort Dir | LastAnyPortNone
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
    | Read1 Inst1 Src

type Inst1 = Add | Sub | JRO | Set | NOP1

type Inst = Mov Src Dst | Inst1 Inst1 Src
type Src = SrcNil | SrcNum Num | SrcAcc | SrcPort SrcPort
type Dst = DstNil | DstAcc | DstPort DstPort


type Msg =  NOP | MsgWithNum Inst1 Num | OnWriteResolved (Maybe LastAnyPort)


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

updateLast: Maybe LastAnyPort -> {a| last: LastAnyPort} -> {a| last: LastAnyPort}
updateLast maybeLast model =
    case maybeLast of
        Just last -> {model | last = last}
        Nothing -> model

step: SrcPortResolver -> NEC -> NEC
step srcPortResolver nec =
    let
        resolveSrc : Src -> Maybe (Num, Maybe LastAnyPort)
        resolveSrc src =
            case srcToNum nec src of
                Ok n -> Just (n, Nothing)
                Err sp -> srcPortResolver sp

        stepInst1 inst1 src =
            case resolveSrc src of
                Just (n, maybeLast) ->
                    update (MsgWithNum inst1 n) nec |> updateLast maybeLast
                Nothing -> { nec | state = Read1 inst1 src, idle = nec.idle + 1 }
    in
    case nec.state of
        Run ->
            case currentInst nec.exe nec.pc of
                Inst1 inst1 src -> stepInst1 inst1 src
                Mov src dst ->
                    case (src,dst) of
                        (SrcPort _, DstNil) -> {nec| state = Read1 NOP1 src}
                        (SrcPort _, DstAcc) -> {nec| state = Read1 Set src}
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


        Read1 inst1 src -> stepInst1 inst1 src

        Read afterReadMsg sp ->
            case srcPortResolver sp of
                Just (n, maybeLast) ->
                    let
                        newNec =
                            case afterReadMsg of
                                WriteAfterRead dp -> {nec| state = Write dp n}
                    in
                        updateLast maybeLast newNec
                Nothing -> nec -- mark this cycle idle.



        Write _ _ ->
            -- on write success, i.e. opp port is in Read from op state
            update NOP nec



type alias SrcPortReader =
    SrcPort -> Maybe (ReadInfo, Context)

type alias ReadInfo =
   (Num, Maybe LastAnyPort)



type alias Context =
   { pending: NodeSet
   , done: Node
   }

type NodeId = NodeId

stepNode: SrcPortReader -> Node -> (Node, Maybe Context)
stepNode _ _ =
    Debug.todo "todo"

readForNode: NodeId -> SrcPort -> NodeSet -> Maybe ((Num, Dir), Node, NodeSet)
readForNode nodeId srcPort nodeSet =
    Debug.todo "todo"


createSrcPortReader: NodeSet -> NodeId -> SrcPortReader
createSrcPortReader set nodeId srcPort =
    Debug.todo "todo"


type NodeSet
    = List Node

type Node = EXENode NodeId NEC | InputNode NodeId | OutputNode NodeId

adjNodeId: NodeId -> Dir -> NodeId
adjNodeId  =
    Debug.todo "todo"

oppDir: Dir -> Dir
oppDir _ =
    Debug.todo "todo"


fromSrcToDst: NodeId -> Dir -> NodeId
fromSrcToDst nodeId dir =
    oppDir dir |> adjNodeId nodeId


readNode: Dir -> Node -> Maybe (Num,Node)
readNode dir node =
    case node of
        EXENode nodeId nec ->
            case nec.state of
                Write dstPort num ->
                    let
                        ans last =
                            Just
                                ( num
                                , EXENode nodeId (update (OnWriteResolved last) nec)
                                )
                    in
                    case dstPort of
                        OutPortDir dir2 ->
                            if dir == dir2 then
                                ans Nothing
                            else
                                Nothing

                        OutPortAny ->
                            ans (Just (LastAnyPort dir))

                _ -> Nothing

        InputNode nodeId ->
            Nothing -- call like above

        OutputNode nodeId ->
            Nothing



