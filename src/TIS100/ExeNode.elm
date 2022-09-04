module TIS100.ExeNode exposing
    ( ExeNode
    , ViewModel(..)
    , compile
    , empty
    , intents
    , state
    , viewModel
    )

import Pivot exposing (Pivot)
import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import TIS100.Ports exposing (Intent(..))
import Utils as U exposing (Dir4(..))


type ExeNode
    = Runnable String (List Intent) State
    | NotRunnable String


type alias Prg =
    Pivot PLine


type alias PLine =
    { no : Int, inst : Inst }


goNext : Prg -> Prg
goNext prg =
    case Pivot.goR prg of
        Just nPrg ->
            nPrg

        Nothing ->
            Pivot.goToStart prg


currInst : Prg -> Inst
currInst prg =
    Pivot.getC prg |> .inst


prgLineNo : Prg -> Int
prgLineNo prg =
    Pivot.lengthL prg


type State
    = ReadyToRun Prg
    | ReadBlocked Prg Dir4 Dst
    | WriteBlocked Prg Dir4 Num


prgFromState : State -> Prg
prgFromState st =
    case st of
        ReadyToRun prg ->
            prg

        ReadBlocked prg _ _ ->
            prg

        WriteBlocked prg _ _ ->
            prg


lineNo : State -> Int
lineNo st =
    prgLineNo (prgFromState st)


type Dst
    = DstPort Dir4
    | DstNil


type Inst
    = Mov Dir4 Dst
    | Nop


compile : String -> Maybe ExeNode
compile srcCode =
    if U.isBlank srcCode then
        Just empty

    else
        srcCode
            |> String.toLower
            |> String.lines
            |> List.indexedMap U.pair
            |> U.reject (U.second >> U.isBlank)
            |> List.map compileLine
            |> U.maybeCombine
            |> Maybe.andThen Pivot.fromList
            |> Maybe.map (init srcCode)


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
            Maybe.map2 Mov
                (parseDir b)
                (parseDst c)

        _ ->
            Nothing


parseDst : String -> Maybe Dst
parseDst token =
    U.maybeOneOf
        [ Maybe.map DstPort (parseDir token)
        , U.maybeFromBool (token == "nil") DstNil
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
    Runnable srcCode (intentsFromPrg prg) (ReadyToRun prg)


intentsFromPrg : Prg -> List Intent
intentsFromPrg prg =
    Pivot.toList prg
        |> List.concatMap (.inst >> intentsFromInst)


intentsFromInst : Inst -> List Intent
intentsFromInst inst =
    case inst of
        Mov f dst ->
            [ Read f ]
                ++ (case dst of
                        DstPort t ->
                            [ Write t ]

                        DstNil ->
                            []
                   )

        Nop ->
            []


empty : ExeNode
empty =
    NotRunnable ""


state : ExeNode -> S.NodeState ExeNode
state exe =
    case exe of
        NotRunnable _ ->
            S.Done

        Runnable sc nts st ->
            case st of
                ReadyToRun prg ->
                    S.ReadyToRun (\() -> Runnable sc nts (run prg))

                ReadBlocked prg f t ->
                    S.ReadBlocked f
                        (writeAfterRead prg t >> Runnable sc nts)

                WriteBlocked prg t num ->
                    S.WriteBlocked num t <|
                        \() ->
                            Runnable sc nts <|
                                ReadyToRun (goNext prg)


run : Prg -> State
run prg =
    case currInst prg of
        Mov f t ->
            ReadBlocked prg f t

        Nop ->
            ReadyToRun (goNext prg)


writeAfterRead : Prg -> Dst -> Num -> State
writeAfterRead prg dst num =
    case dst of
        DstPort t ->
            WriteBlocked prg t num

        DstNil ->
            ReadyToRun (goNext prg)


intents : ExeNode -> List Intent
intents exe =
    case exe of
        NotRunnable _ ->
            []

        Runnable _ nts _ ->
            nts


type ViewModel
    = IDLE String
    | RUNNING String Int


viewModel : ExeNode -> ViewModel
viewModel exe =
    case exe of
        NotRunnable srcCode ->
            IDLE srcCode

        Runnable srcCode _ st ->
            RUNNING srcCode (lineNo st)
