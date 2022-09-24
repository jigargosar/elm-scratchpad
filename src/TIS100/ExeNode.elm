module TIS100.ExeNode exposing
    ( ExeNode
    , ViewModel
    , compile
    , empty
    , instructionCount
    , intents
    , toState
    , viewModel
    )

import Basics.Extra exposing (uncurry)
import TIS100.Num as Num exposing (Num)
import TIS100.Ports exposing (Intent(..))
import TIS100.PuzzlePage.Compiler as Compiler
import TIS100.PuzzlePage.Inst as Inst exposing (..)
import TIS100.PuzzlePage.NodeState as NS
import TIS100.PuzzlePage.Program as Prg exposing (Prg)
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


goNext : Ctx -> Ctx
goNext =
    mapPrg Prg.next


mapPrg : (Prg -> Prg) -> Ctx -> Ctx
mapPrg fn ctx =
    setPrg (fn ctx.prg) ctx


setPrg : Prg -> Ctx -> Ctx
setPrg prg ctx =
    { ctx | prg = prg }


jmpToLabel : String -> Ctx -> Ctx
jmpToLabel lbl =
    mapPrg (Prg.jumpTo lbl)



--addToAccAndGoNext : Num -> Ctx -> Ctx
--addToAccAndGoNext num ctx =
--    { ctx | acc = Num.add ctx.acc num } |> goNext


setAccAndGoNext : Num -> Ctx -> Ctx
setAccAndGoNext num ctx =
    { ctx | acc = num } |> goNext


type State
    = ReadyToRun Ctx
    | ReadBlocked Ctx Dir4 Dst
    | WriteBlocked Ctx Dir4 Num


runnableToState : State -> NS.NodeState State
runnableToState state =
    case state of
        ReadyToRun ctx ->
            NS.ReadyToRun (\() -> run ctx)

        ReadBlocked ctx f t ->
            NS.ReadBlocked f (writeAfterRead ctx t)

        WriteBlocked ctx t num ->
            NS.WriteBlocked num t (\() -> ReadyToRun (goNext ctx))


run : Ctx -> State
run ctx =
    case Prg.inst ctx.prg of
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

        Jmp label ->
            ReadyToRun (jmpToLabel label ctx)

        Jnz label ->
            conditionalJump Num.isNotEqualToZero label ctx

        Jez label ->
            conditionalJump Num.isEqualToZero label ctx

        Jgz label ->
            conditionalJump Num.isGreaterThanZero label ctx

        Jlz label ->
            conditionalJump Num.isLessThanZero label ctx

        Jro _ ->
            Debug.todo "todo"


conditionalJump : (Num -> Bool) -> String -> Ctx -> State
conditionalJump fn label ctx =
    ReadyToRun
        (if fn ctx.acc then
            jmpToLabel label ctx

         else
            goNext ctx
        )


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


compile : String -> Maybe ExeNode
compile srcCode =
    srcCode
        |> Compiler.compile
        |> Result.toMaybe
        |> Maybe.map (init srcCode)


init : String -> Maybe Prg -> ExeNode
init srcCode mbPrg =
    case mbPrg of
        Nothing ->
            NotRunnable srcCode

        Just prg ->
            Runnable srcCode (allIntents prg) (ReadyToRun (initCtx prg))


allIntents : Prg -> List Intent
allIntents prg =
    Prg.allInst prg |> List.concatMap intentsFromInst


intentsFromInst : Inst -> List Intent
intentsFromInst inst =
    Inst.getSrcAndDstOperands inst
        |> Utils.biMap
            (\mbs ->
                case mbs of
                    Just (SrcPort f) ->
                        [ Read f ]

                    _ ->
                        []
            )
            (\mbd ->
                case mbd of
                    Just (DstPort t) ->
                        [ Write t ]

                    _ ->
                        []
            )
        |> uncurry List.append


empty : ExeNode
empty =
    NotRunnable ""


toState : ExeNode -> NS.NodeState ExeNode
toState exe =
    case exe of
        NotRunnable _ ->
            NS.Idle

        Runnable sc nts st ->
            runnableToState st
                |> NS.map (Runnable sc nts)


intents : ExeNode -> List Intent
intents exe =
    case exe of
        NotRunnable _ ->
            []

        Runnable _ nts _ ->
            nts


instructionCount : ExeNode -> Int
instructionCount exeNode =
    case exeNode of
        Runnable _ _ state ->
            ctxFromState state
                |> .prg
                |> Prg.instructionCount

        NotRunnable _ ->
            0


type alias ViewModel =
    { srcCode : String
    , acc : Num
    , mode : String
    , mbCurrentRow : Maybe Int
    }


viewModel : ExeNode -> ViewModel
viewModel exe =
    case exe of
        NotRunnable srcCode ->
            { srcCode = srcCode
            , mbCurrentRow = Nothing
            , acc = Num.zero
            , mode = mode exe
            }

        Runnable srcCode _ st ->
            let
                ctx =
                    ctxFromState st
            in
            { srcCode = srcCode
            , mbCurrentRow = Just (Prg.row ctx.prg)
            , acc = ctx.acc
            , mode = mode exe
            }


mode : ExeNode -> String
mode exe =
    case toState exe of
        NS.ReadyToRun _ ->
            "RUN"

        NS.ReadBlocked _ _ ->
            "READ"

        NS.WriteBlocked _ _ _ ->
            writeModeLabel

        NS.Idle ->
            "IDLE"



--noinspection SpellCheckingInspection


writeModeLabel =
    -- spelling needs to be 4ch for alignment
    "WRTE"
