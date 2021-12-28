module StageJS.BasicGrid exposing (..)

import Random
import Utils exposing (..)


cellsInRow =
    10


cellSize =
    30


gridSize =
    300


type alias Cell =
    { gp : Int2, hue : Float }


cells : List Cell
cells =
    squareGridPositions cellsInRow
        |> List.map (\gp -> Cell gp 0.42)
        |> randomizeAllHues


randomizeAllHues : List Cell -> List Cell
randomizeAllHues cs =
    Random.list (List.length cs) randomNorm
        |> Random.map (List.map2 (\c h -> { c | hue = h }) cs)
        |> stepWithInitialSeed 0


stepWithInitialSeed : Int -> Random.Generator a -> a
stepWithInitialSeed i gen =
    Random.step gen (Random.initialSeed i) |> first


main =
    basicSvg [ viewBoxC gridSize gridSize ]
        [ cells
            |> List.map
                (\cell ->
                    let
                        cc =
                            gpToGridLocal { gridSize = gridSize, cellSize = cellSize } cell.gp
                    in
                    square cellSize [ fill <| hsl cell.hue 1 0.6, xf [ mvT cc ] ]
                )
            |> group []
        ]
