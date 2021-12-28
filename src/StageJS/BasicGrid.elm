module StageJS.BasicGrid exposing (..)

import Anime.Anim as A
import Browser.Events
import Ease
import Random
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { elapsed : Int }


init : () -> ( Model, Cmd Msg )
init () =
    ( { elapsed = 0 }, Cmd.none )


type Msg
    = NOP
    | OnDeltaMS Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (round >> clamp 0 100 >> OnDeltaMS)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnDeltaMS delta ->
            ( { model | elapsed = model.elapsed + delta }, Cmd.none )


view : Model -> Document Msg
view model =
    Document "App Title"
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
                                        ]
                                        (A.clockFromElapsedMillis (model.elapsed - cell.animStart))
                              in
                              transforms
                                [ cell.gp
                                    |> gpToCellCenter
                                    |> mapEach (mul frac)
                                    |> translateF2
                                , scaleF 0.9

                                --, scaleF (0.9 * frac)
                                ]
                            ]
                    )
                |> group []
            ]
        ]


cellsInRow =
    10


cellSize =
    30


gridSize =
    300


type alias Cell =
    { gp : Int2
    , color : String
    , skewX : Float2
    , skewY : Float2
    , animStart : Int
    }


initCellAt : Int2 -> Cell
initCellAt gp =
    { gp = gp
    , color = black
    , skewX = ( 0, 0 )
    , skewY = ( 0, 0 )
    , animStart = 0
    }


cells : List Cell
cells =
    squareGridPositions cellsInRow
        |> List.map initCellAt
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


gpToCellCenter gp =
    gpToGridLocal { gridSize = gridSize, cellSize = cellSize } gp
