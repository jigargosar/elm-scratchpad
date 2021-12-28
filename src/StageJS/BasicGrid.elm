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
    Random.list (List.length cs) randomColor
        |> Random.map (List.map2 (\cell color -> { cell | color = color }) cs)
        |> stepWithInitialSeed 0


randomColor : Generator String
randomColor =
    randomHue |> Random.map (\h -> hsl h 1 0.5)


randomHue : Generator Float
randomHue =
    let
        sampleCount =
            10
    in
    normSamples (sampleCount + 1)
        |> List.drop 1
        |> List.take (sampleCount - 1)
        |> Random.uniform 0


main =
    div []
        [ basicStylesNode
        , basicSvg
            [ viewBoxC gridSize gridSize
            , bgcTransparent
            ]
            [ cells
                |> List.map
                    (\cell ->
                        square cellSize
                            [ fill cell.color
                            , opacity 0.7
                            , transitionTransform
                            , xf [ mvT (gpToCellCenter cell.gp), scale 0.9 ]
                            ]
                    )
                |> group []
            ]
        ]


gpToCellCenter gp =
    gpToGridLocal { gridSize = gridSize, cellSize = cellSize } gp
