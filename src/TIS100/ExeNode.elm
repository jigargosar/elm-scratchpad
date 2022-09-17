module TIS100.ExeNode exposing
    ( ExeNode
    , ViewModel
    , compile
    , empty
    , intents
    , toStepRunnerNodeState
    , viewModel
    )

import Pivot exposing (Pivot)
import TIS100.Num as Num exposing (Num)
import TIS100.Ports exposing (Intent(..))
import TIS100.PuzzlePage.Compiler as Compiler
import TIS100.PuzzlePage.Inst exposing (..)
import TIS100.PuzzlePage.StepRunner as SR
import Utils exposing (Dir4)


type ExeNode
    = Runnable String (List Intent) State
    | NotRunnable String


type alias Ctx =
    { acc : Num
    , prg : Prg
    }


initCtx : Prg -> Ctx
initCtx prg =
    { acc = Num.zero, prg = prg }


type alias Prg =
    Pivot PLine


type alias PLine =
    { lineNo : Int, inst : Inst }


goNext : Ctx -> Ctx
goNext ({ prg } as ctx) =
    case Pivot.goR prg of
        Just nPrg ->
            { ctx | prg = nPrg }

        Nothing ->
            { ctx | prg = Pivot.goToStart prg }



--addToAccAndGoNext : Num -> Ctx -> Ctx
--addToAccAndGoNext num ctx =
--    { ctx | acc = Num.add ctx.acc num } |> goNext


setAccAndGoNext : Num -> Ctx -> Ctx
setAccAndGoNext num ctx =
    { ctx | acc = num } |> goNext


currInst : Ctx -> Inst
currInst { prg } =
    Pivot.getC prg |> .inst


currLineNum : Ctx -> Int
currLineNum { prg } =
    Pivot.getC prg |> .lineNo


type State
    = ReadyToRun Ctx
    | ReadBlocked Ctx Dir4 Dst
    | WriteBlocked Ctx Dir4 Num


stateToStepRunnerNodeState : State -> SR.NodeState State
stateToStepRunnerNodeState state =
    case state of
        ReadyToRun ctx ->
            SR.ReadyToRun (\() -> run ctx)

        ReadBlocked ctx f t ->
            SR.ReadBlocked f (writeAfterRead ctx t)

        WriteBlocked ctx t num ->
            SR.WriteBlocked num t (\() -> ReadyToRun (goNext ctx))


run : Ctx -> State
run ctx =
    case currInst ctx of
        Mov src dst ->
            case src of
                SrcPort dir ->
                    ReadBlocked ctx dir dst

                SrcAcc ->
                    writeAfterRead ctx dst ctx.acc

                SrcNum num ->
                    writeAfterRead ctx dst num

        Nop ->
            ReadyToRun (goNext ctx)


writeAfterRead : Ctx -> Dst -> Num -> State
writeAfterRead ctx dst num =
    case dst of
        DstPort dir ->
            WriteBlocked ctx dir num

        DstNil ->
            ReadyToRun (goNext ctx)

        DstAcc ->
            ReadyToRun (setAccAndGoNext num ctx)


ctxFromState : State -> Ctx
ctxFromState st =
    case st of
        ReadyToRun prg ->
            prg

        ReadBlocked prg _ _ ->
            prg

        WriteBlocked prg _ _ ->
            prg


compile : String -> Result Compiler.Errors ExeNode
compile srcCode =
    srcCode
        |> Compiler.compile
        |> Result.map (List.map prgLineFromTuple)
        |> Result.map (init srcCode)


prgLineFromTuple : ( Int, Inst ) -> PLine
prgLineFromTuple ( a, b ) =
    PLine a b


init : String -> List PLine -> ExeNode
init srcCode prgLines =
    case Pivot.fromList prgLines of
        Nothing ->
            NotRunnable srcCode

        Just prg ->
            Runnable srcCode (intentsFromPrg prg) (ReadyToRun (initCtx prg))


intentsFromPrg : Prg -> List Intent
intentsFromPrg prg =
    Pivot.toList prg
        |> List.concatMap (.inst >> intentsFromInst)


intentsFromInst : Inst -> List Intent
intentsFromInst inst =
    case inst of
        Mov src dst ->
            (case src of
                SrcPort f ->
                    [ Read f ]

                _ ->
                    []
            )
                ++ (case dst of
                        DstPort t ->
                            [ Write t ]

                        _ ->
                            []
                   )

        Nop ->
            []


empty : ExeNode
empty =
    NotRunnable ""


toStepRunnerNodeState : ExeNode -> SR.NodeState ExeNode
toStepRunnerNodeState exe =
    case exe of
        NotRunnable _ ->
            SR.Done

        Runnable sc nts st ->
            stateToStepRunnerNodeState st
                |> SR.map (Runnable sc nts)


intents : ExeNode -> List Intent
intents exe =
    case exe of
        NotRunnable _ ->
            []

        Runnable _ nts _ ->
            nts


type alias ViewModel =
    { srcCode : String
    , acc : Num
    , mode : String
    , maybeLineNo : Maybe Int
    }


viewModel : ExeNode -> ViewModel
viewModel exe =
    case exe of
        NotRunnable srcCode ->
            { srcCode = srcCode
            , maybeLineNo = Nothing
            , acc = Num.zero
            , mode = mode exe
            }

        Runnable srcCode _ st ->
            let
                ctx =
                    ctxFromState st
            in
            { srcCode = srcCode
            , maybeLineNo = Just (currLineNum ctx)
            , acc = ctx.acc
            , mode = mode exe
            }


mode : ExeNode -> String
mode exe =
    case toStepRunnerNodeState exe of
        SR.ReadyToRun _ ->
            "RUN"

        SR.ReadBlocked _ _ ->
            "READ"

        SR.WriteBlocked _ _ _ ->
            writeModeLabel

        SR.Done ->
            "IDLE"



--noinspection SpellCheckingInspection


writeModeLabel =
    -- spelling needs to be 4ch for alignment
    "WRTE"
