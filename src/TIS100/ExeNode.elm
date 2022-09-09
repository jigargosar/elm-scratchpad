module TIS100.ExeNode exposing
    ( ExeNode
    , ViewModel
    , compile
    , empty
    , intents
    , toStepRunnerNodeState
    , viewModel
    )

import Parser exposing ((|.), (|=), Parser)
import Pivot exposing (Pivot)
import Set
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
    let
        _ =
            let
                v =
                    Parser.run instListParser " mov"
            in
            Debug.log "Debug: " v
    in
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


type Stmt
    = Label String
    | LabeledInst String Inst


statementsParser =
    Parser.loop [] statementsParserHelp


statementsParserHelp revList =
    Parser.oneOf
        [ Parser.succeed (\stmt -> Parser.Loop (stmt :: revList))
            |= stmtParser
            |. Parser.spaces
        , Parser.succeed (\_ -> Parser.Done (List.reverse revList))
            |= Parser.end
        ]


stmtParser : Parser Stmt
stmtParser =
    Parser.oneOf
        [ Parser.succeed LabeledInst
            |. spaces
            |= prefixLabelParser
            |. spaces
            |= instParser
        , Parser.succeed Label
            |. spaces
            |= prefixLabelParser
        ]


prefixLabelParser : Parser String
prefixLabelParser =
    Parser.succeed identity
        |= labelParser
        |. spaces
        |. Parser.symbol ":"


labelParser : Parser String
labelParser =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


spaces =
    Parser.chompWhile (\c -> c == ' ')


instListParser : Parser (List Inst)
instListParser =
    Parser.loop [] instListParserHelp


instListParserHelp : List Inst -> Parser (Parser.Step (List Inst) (List Inst))
instListParserHelp reverseList =
    Parser.oneOf
        [ Parser.succeed (\inst -> Parser.Loop (inst :: reverseList))
            |= instParser
            |. Parser.spaces
        , Parser.succeed (\_ -> Parser.Done (List.reverse reverseList))
            |= Parser.end
        ]


instParser : Parser Inst
instParser =
    Parser.oneOf
        [ movInstParser
        , nopInstParser
        ]


nopInstParser : Parser Inst
nopInstParser =
    simpleKeyword "nop" Nop


movInstParser : Parser Inst
movInstParser =
    Parser.succeed Mov
        |. Parser.keyword "mov"
        |. Parser.spaces
        |= srcParser
        |. Parser.spaces
        |= dstParser


dstParser : Parser Dst
dstParser =
    Parser.oneOf
        [ Parser.map DstPort dirParser
        , simpleKeyword "acc" DstAcc
        , simpleKeyword "nil" DstNil
        ]


srcParser : Parser Src
srcParser =
    Parser.oneOf
        [ Parser.map SrcPort dirParser
        , simpleKeyword "acc" SrcAcc
        , Parser.map SrcNum numParser
        ]


numParser : Parser Num
numParser =
    Parser.succeed Num.fromInt
        |= signedIntParser


signedIntParser : Parser Int
signedIntParser =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


dirParser : Parser Dir4
dirParser =
    Parser.oneOf
        [ simpleKeyword "left" Left
        , simpleKeyword "right" Right
        , simpleKeyword "up" Up
        , simpleKeyword "down" Down
        ]


simpleKeyword : String -> a -> Parser a
simpleKeyword string a =
    Parser.succeed a |. Parser.keyword string


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
