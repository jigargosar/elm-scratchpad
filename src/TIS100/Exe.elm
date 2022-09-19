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
import TIS100.Num as Num exposing (Num)
import TIS100.Ports exposing (Intent(..))
import TIS100.PuzzlePage.Compiler as Compiler
import TIS100.PuzzlePage.Inst exposing (..)
import TIS100.PuzzlePage.NodeState as NS
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

        Jmp string ->
            Debug.todo "todo"


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
        |> Maybe.map (List.map prgLineFromTuple)
        |> Maybe.map (init srcCode)


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
