module StageJS.BasicGrid exposing (main)

import Anime.Anim as A
import Browser.Events
import Ease
import Html.Events
import Json.Decode as JD
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
    { elapsed : Int
    , cells : List Cell
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        cells : List Cell
        cells =
            squareGridPositions cellsInRow
                |> List.map initCellAt
                |> randomizeAllHues
    in
    ( { elapsed = 0
      , cells = cells
      }
    , Cmd.none
    )


type Msg
    = NOP
    | OnDeltaMS Int
    | MouseEnteredGP Int2


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

        MouseEnteredGP gp ->
            ( { model
                | cells =
                    List.map
                        (\cell ->
                            if cell.gp == gp then
                                { cell
                                    | animStart = model.elapsed
                                    , skewX = swap cell.skewX
                                    , skewY = swap cell.skewY
                                }

                            else
                                cell
                        )
                        model.cells
              }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    Document "App Title"
        [ basicStylesNode
        , basicSvg
            [ viewBoxC gridSize gridSize
            , bgcTransparent
            ]
            [ model.cells
                |> List.map
                    (\cell ->
                        square cellSize
                            [ fill cell.color
                            , opacity 0.7
                            , let
                                clock =
                                    A.clockFromElapsedMillis (model.elapsed - cell.animStart)

                                commonAttrs =
                                    [ A.duration 1800
                                    , A.ease Ease.inOutSine
                                    , A.alternateDirection
                                    ]

                                skewX =
                                    A.value (A.fromToF2 cell.skewX :: commonAttrs) clock

                                rotation =
                                    tweenValueAt initialTweenClock cell.rotation
                              in
                              transforms
                                [ cell.gp
                                    |> gpToCellCenter
                                    --|> mapEach (mul (A.value commonAttrs clock))
                                    |> translateF2
                                , "skewX(" ++ fTurn skewX ++ ")"
                                , "rotate(" ++ fDeg rotation ++ ")"
                                , scaleF 0.9

                                --, scaleF (0.9 * frac)
                                ]
                            , Html.Events.on "mouseenter" (JD.succeed (MouseEnteredGP cell.gp))
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


type alias Tween =
    { from : Float
    , to : Float
    , duration : Int
    , start : TweenClock
    }


tween : Float -> Tween
tween val =
    { from = val
    , to = val
    , duration = 0
    , start = initialTweenClock
    }


type TweenAttr
    = TweenDuration Int


type TweenClock
    = TweenClock Int


initialTweenClock =
    TweenClock 0


tweenTo : Float -> List TweenAttr -> TweenClock -> Tween -> Tween
tweenTo to attrs clock t =
    { t | to = to, from = tweenValueAt clock t, start = clock }
        |> applyTweenAttrs attrs


applyTweenAttrs : List TweenAttr -> Tween -> Tween
applyTweenAttrs attrs tInitial =
    let
        applyAttr a t =
            case a of
                TweenDuration ms ->
                    { t | duration = ms }
    in
    List.foldl applyAttr tInitial attrs


tweenValueAt : TweenClock -> Tween -> Float
tweenValueAt clock t =
    t.to


type alias Cell =
    { gp : Int2
    , color : String
    , skewX : Float2
    , skewY : Float2
    , rotation : Tween
    , animStart : Int
    }


initCellAt : Int2 -> Cell
initCellAt gp =
    { gp = gp
    , color = black
    , skewX = ( 0, 0.15 )
    , skewY = ( 0, 0.15 )
    , rotation = tween 0 |> tweenTo 360 [ TweenDuration 1800 ] initialTweenClock
    , animStart = 0
    }


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
