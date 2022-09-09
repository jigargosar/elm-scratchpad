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
import TIS100.PuzzlePage.StepRunner as SR
import Utils as U exposing (Dir4(..))


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


prgFromList : List PLine -> Maybe (Pivot PLine)
prgFromList =
    Pivot.fromList


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


ctxFromState : State -> Ctx
ctxFromState st =
    case st of
        ReadyToRun prg ->
            prg

        ReadBlocked prg _ _ ->
            prg

        WriteBlocked prg _ _ ->
            prg


type Dst
    = DstPort Dir4
    | DstAcc
    | DstNil


type Src
    = SrcPort Dir4
    | SrcAcc
    | SrcNum Num


type Inst
    = Mov Src Dst
    | Nop


compile : String -> Result String ExeNode
compile srcCode =
    if U.isBlank srcCode then
        Ok empty

    else
        srcCode
            |> String.toLower
            |> String.lines
            |> List.indexedMap U.pair
            |> U.reject (U.second >> U.isBlank)
            |> List.map compileLine
            |> U.maybeCombine
            |> Maybe.andThen prgFromList
            |> Maybe.map (init srcCode)
            |> Result.fromMaybe "Compilation Failed"


toTokens : String -> List String
toTokens line =
    String.split " " line
        |> U.reject (U.eq "")


compileLine : ( Int, String ) -> Maybe PLine
compileLine ( no, line ) =
    parseInst line |> Maybe.map (PLine no)


parseInst : String -> Maybe Inst
parseInst line =
    case toTokens line of
        "mov" :: b :: c :: [] ->
            Maybe.map2 Mov (parseSrc b) (parseDst c)

        "nop" :: [] ->
            Just Nop

        _ ->
            Nothing


parseDst : String -> Maybe Dst
parseDst token =
    U.maybeOneOf
        [ Maybe.map DstPort (parseDir token)
        , U.maybeFromBool (token == "nil") DstNil
        , U.maybeFromBool (token == "acc") DstAcc
        ]


parseSrc : String -> Maybe Src
parseSrc token =
    U.maybeOneOf
        [ Maybe.map SrcPort (parseDir token)
        , U.maybeFromBool (token == "acc") SrcAcc
        , Maybe.map SrcNum (Num.parse token)
        ]


parseDir : String -> Maybe Dir4
parseDir string =
    case string of
        "left" ->
            Just Left

        "right" ->
            Just Right

        "up" ->
            Just Up

        "down" ->
            Just Down

        _ ->
            Nothing


init : String -> Prg -> ExeNode
init srcCode prg =
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
            case st of
                ReadyToRun ctx ->
                    SR.ReadyToRun (\() -> Runnable sc nts (run ctx))

                ReadBlocked ctx f t ->
                    SR.ReadBlocked f
                        (writeAfterRead ctx t >> Runnable sc nts)

                WriteBlocked ctx t num ->
                    SR.WriteBlocked num t <|
                        \() ->
                            Runnable sc nts <|
                                ReadyToRun (goNext ctx)


stateToStepRunnerNodeState : State -> SR.NodeState State
stateToStepRunnerNodeState state =
    case state of
        ReadyToRun ctx ->
            SR.ReadyToRun (\() -> run ctx)

        ReadBlocked ctx f t ->
            SR.ReadBlocked f
                (writeAfterRead ctx t)

        WriteBlocked ctx t num ->
            SR.WriteBlocked num t <|
                \() -> ReadyToRun (goNext ctx)


run : Ctx -> State
run ctx =
    case currInst ctx of
        Mov src dst ->
            case src of
                SrcPort f ->
                    ReadBlocked ctx f dst

                SrcAcc ->
                    writeAfterRead ctx dst ctx.acc

                SrcNum num ->
                    writeAfterRead ctx dst num

        Nop ->
            ReadyToRun (goNext ctx)


writeAfterRead : Ctx -> Dst -> Num -> State
writeAfterRead ctx dst num =
    case dst of
        DstPort t ->
            WriteBlocked ctx t num

        DstNil ->
            ReadyToRun (goNext ctx)

        DstAcc ->
            ReadyToRun (setAccAndGoNext num ctx)


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
