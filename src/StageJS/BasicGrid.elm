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
    { gp : Int2, color : String }


cells : List Cell
cells =
    squareGridPositions cellsInRow
        |> List.map (\gp -> Cell gp black)
        |> randomizeAllHues


randomizeAllHues : List Cell -> List Cell
randomizeAllHues cs =
    let
        sampleCount =
            10

        randomHue : Generator Float
        randomHue =
            normSamples (sampleCount + 1)
                |> List.drop 1
                |> List.take (sampleCount - 1)
                |> Random.uniform 0
    in
    Random.list (List.length cs) randomHue
        |> Random.map (List.map2 (\c h -> { c | color = hsl h 0.9 0.55 }) cs)
        |> stepWithInitialSeed 0


main =
    basicSvg [ viewBoxC gridSize gridSize ]
        [ cells
            |> List.map
                (\cell ->
                    let
                        cc =
                            gpToGridLocal { gridSize = gridSize, cellSize = cellSize } cell.gp
                    in
                    square cellSize [ fill cell.color, xf [ mvT cc ] ]
                )
            |> group []
        ]
