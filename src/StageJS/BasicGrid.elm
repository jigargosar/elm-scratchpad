module StageJS.BasicGrid exposing (..)

import Utils exposing (..)


gc =
    { gridSize = 10, cellSize = 10 }


main =
    basicSvg []
        [ squareGridPositions gc.gridSize
            |> List.map
                (\gp ->
                    square gc.cellSize [ fill green, xf [ mvT (gpToGridLocal gc gp) ] ]
                )
            |> group []
        ]
