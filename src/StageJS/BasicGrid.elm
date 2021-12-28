module StageJS.BasicGrid exposing (..)

import Utils exposing (..)


cellsInRow =
    10


cellSize =
    30


gridSize =
    300


gc : { gridSize : Float, cellSize : Float }
gc =
    { gridSize = gridSize, cellSize = cellSize }


main =
    basicSvg [ viewBoxC gc.gridSize gc.gridSize ]
        [ squareGridPositions cellsInRow
            |> List.map
                (\gp ->
                    square gc.cellSize [ fill green, xf [ mvT (gpToGridLocal gc gp) ] ]
                )
            |> group []
        ]
