module TIS100.PuzzlePage.Program exposing (Prg, PrgLine, allInst, fromList, inst, jumpTo, next, row)

import Pivot exposing (Pivot)
import Set exposing (Set)
import TIS100.PuzzlePage.Inst exposing (Inst)
import Utils


type alias PrgLine =
    { lineNo : Int
    , labels : Set String
    , inst : Inst
    }


type Prg
    = Prg (Pivot PrgLine)


fromList : List PrgLine -> Maybe Prg
fromList ls =
    Pivot.fromList ls |> Maybe.map Prg


map fn (Prg prg) =
    fn prg |> Prg


next : Prg -> Prg
next =
    map <|
        \prg ->
            case Pivot.goR prg of
                Just nPrg ->
                    nPrg

                Nothing ->
                    Pivot.goToStart prg


jumpTo : String -> Prg -> Prg
jumpTo lbl =
    map <|
        Pivot.withRollback
            (cyclicFindFromCenter
                (.labels >> Set.member lbl)
            )


cyclicFindFromCenter : (a -> Bool) -> Pivot a -> Maybe (Pivot a)
cyclicFindFromCenter pred pivot =
    Pivot.findCR pred pivot
        |> Utils.orElseLazy
            (\_ ->
                Pivot.firstWith pred pivot
            )


current : Prg -> PrgLine
current (Prg prg) =
    Pivot.getC prg


inst : Prg -> Inst
inst =
    current >> .inst


row : Prg -> Int
row =
    current >> .lineNo


allInst : Prg -> List Inst
allInst (Prg prg) =
    Pivot.toList prg |> List.map .inst
