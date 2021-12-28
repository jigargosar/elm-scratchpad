module StageJS.BasicGrid exposing (..)

import Utils exposing (..)


cellsInRow =
    10


cellSize =
    30


gridSize =
    300


main =
    basicSvg [ viewBoxC gridSize gridSize ]
        [ squareGridPositions cellsInRow
            |> List.map
                (\gp ->
                    let
                        cc =
                            gpToGridLocal { gridSize = gridSize, cellSize = cellSize } gp
                    in
                    square cellSize
                        [ fill green
                        , xf [ mvT cc ]
                        ]
                )
            |> group []
        ]
