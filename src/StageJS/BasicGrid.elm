module StageJS.BasicGrid exposing (..)

import Utils exposing (..)


gc =
    { gridSize = 300, cellSize = 30 }


main =
    basicSvg [ viewBoxC gc.gridSize gc.gridSize ]
        [ squareGridPositions (gc.gridSize // gc.cellSize)
            |> List.map
                (\gp ->
                    square gc.cellSize [ fill green, xf [ mvT (gpToGridLocal gc gp) ] ]
                )
            |> group []
        ]
