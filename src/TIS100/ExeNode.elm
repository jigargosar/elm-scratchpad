module TIS100.ExeNode exposing
    ( ExeNode
    , compile
    , empty
    , initMov
    , initMovUpDown
    , intents
    , state
    , toSource
    )

import TIS100.NodeState as S
import TIS100.Num exposing (Num)
import TIS100.Ports exposing (Intent(..))
import Utils as U exposing (Dir4(..))


type ExeNode
    = Runnable Inst State
    | NotRunnable


type State
    = ReadyToRun
    | ReadBlocked Dir4 Dir4
    | WriteBlocked Dir4 Num


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
                    Maybe.map2 initMov
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


initMovUpDown : ExeNode
initMovUpDown =
    initMov Up Down


initMov : Dir4 -> Dir4 -> ExeNode
initMov f t =
    Runnable (Mov f t) ReadyToRun


empty : ExeNode
empty =
    NotRunnable


state : ExeNode -> S.NodeState ExeNode
state exe =
    case exe of
        NotRunnable ->
            S.Done

        Runnable inst state_ ->
            case state_ of
                ReadyToRun ->
                    S.ReadyToRun (\() -> Runnable inst (run inst))

                ReadBlocked f t ->
                    S.ReadBlocked f (WriteBlocked t >> Runnable inst)

                WriteBlocked t num ->
                    S.WriteBlocked num t (\() -> Runnable inst ReadyToRun)


run : Inst -> State
run inst =
    case inst of
        Mov f t ->
            ReadBlocked f t

        Nop ->
            ReadyToRun


intents : ExeNode -> List Intent
intents exe =
    case exe of
        NotRunnable ->
            []

        Runnable inst _ ->
            case inst of
                Mov f t ->
                    [ Read f, Write t ]

                Nop ->
                    []


toSource : ExeNode -> String
toSource exe =
    case exe of
        NotRunnable ->
            ""

        Runnable i _ ->
            Debug.toString i
