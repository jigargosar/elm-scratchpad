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
    Pivot Inst


type State
    = ReadyToRun Prg
    | ReadBlocked Prg Dir4 Dir4
    | WriteBlocked Prg Dir4 Num


type Inst
    = Mov Dir4 Dir4
    | Nop


compile : String -> Maybe ExeNode
compile srcCode =
    let
        toTokens : String -> List String
        toTokens line =
            String.split " " line
                |> U.reject (U.eq "")

        dirFromString : String -> Maybe Dir4
        dirFromString string =
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

        compileLine : String -> Maybe ExeNode
        compileLine line =
            case toTokens line of
                "mov" :: b :: c :: [] ->
                    Maybe.map2 (initMov srcCode)
                        (dirFromString b)
                        (dirFromString c)

                _ ->
                    Nothing
    in
    if U.isBlank srcCode then
        Just empty

    else
        srcCode
            |> String.toLower
            |> String.lines
            |> List.map String.trim
            |> List.head
            |> Maybe.andThen compileLine


initMov : String -> Dir4 -> Dir4 -> ExeNode
initMov srcCode f t =
    let
        prg =
            Pivot.singleton (Mov f t)
    in
    Runnable srcCode (intentsFromPrg prg) (ReadyToRun prg)


intentsFromPrg : Prg -> List Intent
intentsFromPrg prg =
    Pivot.toList prg
        |> List.concatMap intentsFromInst


intentsFromInst : Inst -> List Intent
intentsFromInst inst =
    case inst of
        Mov f t ->
            [ Read f, Write t ]

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
                        (WriteBlocked prg t >> Runnable sc nts)

                WriteBlocked prg t num ->
                    S.WriteBlocked num t <|
                        \() ->
                            Runnable sc nts <|
                                ReadyToRun (prgNext prg)


prgNext : Prg -> Prg
prgNext prg =
    case Pivot.goR prg of
        Just nPrg ->
            nPrg

        Nothing ->
            Pivot.goToStart prg


run : Prg -> State
run prg =
    case Pivot.getC prg of
        Mov f t ->
            ReadBlocked prg f t

        Nop ->
            ReadyToRun (prgNext prg)


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

        Runnable srcCode _ _ ->
            RUNNING srcCode 0
