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
    { gp : Int2 }


cells : List Cell
cells =
    squareGridPositions cellsInRow
        |> List.map
            (\gp -> Cell gp)


main =
    basicSvg [ viewBoxC gridSize gridSize ]
        [ cells
            |> List.map
                (\cell ->
                    let
                        cc =
                            gpToGridLocal { gridSize = gridSize, cellSize = cellSize } cell.gp
                    in
                    square cellSize [ fill green, xf [ mvT cc ] ]
                )
            |> group []
        ]


randomColor : Generator String
randomColor =
    randomNorm |> Random.map (\h -> hsl h 1 0.5)
