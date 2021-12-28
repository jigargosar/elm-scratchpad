module StageJS.BasicGrid exposing (..)

import Anime.Anim as A
import Browser
import Browser.Events
import Ease
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
    animationApp (round >> A.clockFromMillis >> view)


view clock =
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
                            , let
                                frac =
                                    A.value
                                        [ A.duration 1800
                                        , A.ease Ease.inOutExpo
                                        , A.alternateDirection
                                        , A.loopForever
                                        ]
                                        clock
                              in
                              transforms
                                [ cell.gp
                                    |> gpToCellCenter
                                    |> mapEach (mul frac)
                                    |> translateF2

                                --, scaleF (0.9 * frac)
                                , scaleF 0.9
                                ]
                            ]
                    )
                |> group []
            ]
        ]


gpToCellCenter gp =
    gpToGridLocal { gridSize = gridSize, cellSize = cellSize } gp


animationApp : (Float -> Html Float) -> Program () Float Float
animationApp vfn =
    Browser.element
        { init = \() -> ( 0, Cmd.none )
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta (clamp 0 100)
        , update = \delta elapsed -> ( elapsed + delta, Cmd.none )
        , view = vfn
        }
