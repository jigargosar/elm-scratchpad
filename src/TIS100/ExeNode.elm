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
    if U.isBlank srcCode then
        Ok empty

    else
        srcCode
            |> String.toLower
            |> Parser.run statementsParser
            |> Result.map (List.filterMap prgLineFromStmt)
            |> Result.map (init2 srcCode)
            |> Result.mapError
                (\des ->
                    let
                        _ =
                            des
                                |> Debug.log "Debug: "
                    in
                    des
                        |> List.head
                        |> Maybe.map (.problem >> Debug.toString)
                        |> Maybe.withDefault "Compilation Failed"
                )


type Stmt
    = Label String
    | LabeledInst String ( Int, Inst )
    | OnlyInst ( Int, Inst )


prgLineFromStmt : Stmt -> Maybe PLine
prgLineFromStmt stmt =
    case stmt of
        Label _ ->
            Nothing

        LabeledInst _ ( lineNo, inst ) ->
            Just <| PLine lineNo inst

        OnlyInst ( lineNo, inst ) ->
            Just <| PLine lineNo inst


statementsParser : Parser (List Stmt)
statementsParser =
    Parser.loop [] statementsParserHelp


statementsParserHelp : List Stmt -> Parser (Parser.Step (List Stmt) (List Stmt))
statementsParserHelp revStmts =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed (\stmt -> Parser.Loop (stmt :: revStmts))
                |= stmtParser
            , Parser.succeed (\_ -> Parser.Done (List.reverse revStmts))
                |= Parser.end
            ]


stmtParser : Parser Stmt
stmtParser =
    Parser.oneOf
        [ prefixLabelParser
            |> Parser.andThen labeledStmtParser
        , Parser.succeed OnlyInst
            |= instParser
        ]


labeledStmtParser : String -> Parser Stmt
labeledStmtParser label =
    Parser.oneOf
        [ Parser.succeed (Label label)
            |. stmtEndChar
        , Parser.succeed (LabeledInst label)
            |= instParser
        ]


stmtEndChar : Parser ()
stmtEndChar =
    Parser.oneOf [ Parser.end, Parser.symbol "\n" ]


stmtEnd : Parser ()
stmtEnd =
    Parser.succeed ()
        |. spaceChars
        |. Parser.oneOf [ Parser.end, Parser.symbol "\n" ]


prefixLabelParser : Parser String
prefixLabelParser =
    Parser.succeed identity
        |= labelParser
        |. spaceChars
        |. Parser.symbol ":"
        |. spaceChars


labelParser : Parser String
labelParser =
    Parser.variable
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = reservedKeywords
        }


reservedKeywords : Set.Set String
reservedKeywords =
    Set.fromList
        [ "nop"
        , "mov"
        , "add"
        , "sub"
        , "sav"
        , "jmp"
        , "jez"
        , "jgz"
        , "jlz"
        , "jro"
        ]


spaceChars : Parser ()
spaceChars =
    Parser.chompWhile (\c -> c == ' ')


instParser : Parser ( Int, Inst )
instParser =
    withLineNum <|
        Parser.oneOf
            [ movInstParser
            , nopInstParser
            ]
            |. stmtEnd


withLineNum : Parser a -> Parser ( Int, a )
withLineNum parser =
    Parser.succeed U.pair
        |= (Parser.getRow |> Parser.map U.dec)
        |= parser


nopInstParser : Parser Inst
nopInstParser =
    simpleKeyword "nop" Nop


movInstParser : Parser Inst
movInstParser =
    Parser.succeed Mov
        |. Parser.keyword "mov"
        |. spaceChars
        |= srcParser
        |. spaceChars
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


init2 : String -> List PLine -> ExeNode
init2 srcCode prgLines =
    case Pivot.fromList prgLines of
        Nothing ->
            NotRunnable srcCode

        Just prg ->
            Runnable srcCode (intentsFromPrg prg) (ReadyToRun (initCtx prg))


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
