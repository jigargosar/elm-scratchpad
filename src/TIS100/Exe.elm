module TIS100.Exe exposing
    ( ExeNode
    , ViewModel
    , compile
    , empty
    , intents
    , toState
    , viewModel
    )

import Pivot exposing (Pivot)
import Set exposing (Set)
import TIS100.Num as Num exposing (Num)
import TIS100.Ports exposing (Intent(..))
import TIS100.PuzzlePage.Compiler as Compiler
import TIS100.PuzzlePage.Inst exposing (..)
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


mapPrgMaybe : (Prg -> Maybe Prg) -> Ctx -> Ctx
mapPrgMaybe fn ctx =
    case fn ctx.prg of
        Just prg ->
            setPrg prg ctx

        Nothing ->
            ctx


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
            Runnable srcCode (intentsFromPrg prg) (ReadyToRun (initCtx prg))


intentsFromPrg : Prg -> List Intent
intentsFromPrg prg =
    Prg.allInst prg |> List.concatMap intentsFromInst


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

        Jmp _ ->
            []


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
