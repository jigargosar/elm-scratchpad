module StageJS.BasicGrid exposing (main)

import Browser.Events
import Html.Events
import Json.Decode as JD
import Random exposing (Seed)
import Random.Extra
import Utils exposing (..)



{-
   TODO:
   * fix w colors
   * randomize all props

-}


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { clock : TweenClock
    , cells : List Cell
    , seed : Seed
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
    ( { clock = initialTweenClock
      , cells = cells
      , seed = Random.initialSeed 0
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
            ( { model | clock = updateClock delta model.clock }
            , Cmd.none
            )

        MouseEnteredGP gp ->
            let
                cellsGen =
                    List.map
                        (\cell ->
                            if cell.gp == gp then
                                randomizeCell model.clock cell

                            else
                                Random.constant cell
                        )
                        model.cells
                        |> Random.Extra.combine

                ( cells, seed ) =
                    Random.step cellsGen model.seed
            in
            ( { model | cells = cells, seed = seed }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    Document "App Title"
        [ basicStylesNode
        , basicSvg
            [ viewBoxC gridSize gridSize
            , bgcTransparent
            , overflowVisible
            ]
            [ model.cells
                |> List.map
                    (\cell ->
                        square cellSize
                            [ fill cell.color
                            , opacity 0.7
                            , let
                                skewX =
                                    0

                                rotation =
                                    tweenValueAt model.clock cell.rotation
                              in
                              transforms
                                [ cell.gp
                                    |> gpToCellCenter
                                    --|> mapEach (mul (A.value commonAttrs clock))
                                    |> translateF2
                                , "skewX(" ++ fDeg skewX ++ ")"
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


updateClock : Int -> TweenClock -> TweenClock
updateClock ms (TweenClock elapsed) =
    elapsed + ms |> TweenClock


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
tweenValueAt (TweenClock now) t =
    let
        (TweenClock start) =
            t.start

        elapsed =
            now - start
    in
    --if elapsed <= 0 then
    --    t.from
    --
    --else if elapsed >= t.duration then
    --    t.to
    --
    --else
    (toFloat elapsed / toFloat t.duration)
        |> clamp 0 1
        |> lerp t.from t.to


type alias Cell =
    { gp : Int2
    , color : String
    , skewX : Tween
    , skewY : Tween
    , rotation : Tween
    }


initCellAt : Int2 -> Cell
initCellAt gp =
    { gp = gp
    , color = black
    , skewX = tween 0 |> tweenTo 15 [ TweenDuration 1800 ] initialTweenClock
    , skewY = tween 0 |> tweenTo 15 [ TweenDuration 1800 ] initialTweenClock
    , rotation = tween 0 |> tweenTo 360 [ TweenDuration 1800 ] initialTweenClock
    }


randomizeCell : TweenClock -> Cell -> Generator Cell
randomizeCell clock cell =
    Random.constant (Cell cell.gp)
        |> Random.Extra.andMap randomColor
        |> Random.Extra.andMap
            (cell.skewX
                |> tweenTo 15 [ TweenDuration 1800 ] clock
                |> Random.constant
            )
        |> Random.Extra.andMap
            (cell.skewY
                |> tweenTo 15 [ TweenDuration 1800 ] clock
                |> Random.constant
            )
        |> Random.Extra.andMap
            (Random.map2
                (\r d ->
                    cell.rotation
                        |> tweenTo r [ TweenDuration d ] clock
                )
                (Random.float -360 360)
                (Random.int 1000 2000)
            )


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
