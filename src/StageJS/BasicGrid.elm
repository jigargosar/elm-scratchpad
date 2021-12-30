module StageJS.BasicGrid exposing (main)

import Browser.Events
import Html.Events
import Json.Decode as JD
import Random exposing (Seed)
import Utils exposing (..)



{-
   TODO:
   [x] randomize all props
   * fix w colors



   MAYBE:
   * refactor list item randomization process (in update)
   * remove initial color's and rotation's, or perhaps just rotation.

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
        clock =
            initialTweenClock

        initialSeed =
            Random.initialSeed 0

        randomGridCells : Generator (List Cell)
        randomGridCells =
            squareGridPositions cellsInRow
                |> List.map (initCellAt >> randomizeCell clock)
                |> rCombine

        ( cells, seed ) =
            Random.step randomGridCells initialSeed
    in
    ( { clock = clock
      , cells = cells
      , seed = seed
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
                        |> rCombine

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
                |> List.map (viewCell model.clock)
                |> group []
            ]
        ]


viewCell : TweenClock -> Cell -> Svg Msg
viewCell clock cell =
    square cellSize
        [ fill cell.color
        , opacity 0.7
        , let
            scaleX =
                tweenValueAt clock cell.scaleX

            scaleY =
                tweenValueAt clock cell.scaleY

            skewX =
                tweenValueAt clock cell.skewX

            skewY =
                tweenValueAt clock cell.skewY

            rotation =
                tweenValueAt clock cell.rotation
          in
          transforms
            [ cell.gp
                |> gpToCellCenter
                --|> mapEach (mul (A.value commonAttrs clock))
                |> translateF2
            , scaleF 0.9
            , "scaleX(" ++ fromFloat scaleX ++ ")"
            , "scaleY(" ++ fromFloat scaleY ++ ")"
            , "skewX(" ++ fRad skewX ++ ")"
            , "skewY(" ++ fRad skewY ++ ")"
            , "rotate(" ++ fRad rotation ++ ")"

            --, scaleF (0.9 * frac)
            ]
        , Html.Events.on "mouseenter" (JD.succeed (MouseEnteredGP cell.gp))
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
    , scaleX : Tween
    , scaleY : Tween
    , rotation : Tween
    , skewX : Tween
    , skewY : Tween
    }


initCellAt : Int2 -> Cell
initCellAt gp =
    { gp = gp
    , color = black
    , scaleX = tween 1
    , scaleY = tween 1
    , rotation = tween 0
    , skewX = tween 0
    , skewY = tween 0
    }


randomizeCell : TweenClock -> Cell -> Generator Cell
randomizeCell clock cell =
    let
        randomInInterval : Float2 -> Generator Float
        randomInInterval ( a, b ) =
            Random.float a b
    in
    Random.constant
        (\duration color scaleX scaleY rotation skewX skewY ->
            let
                tweenToHelp to =
                    tweenTo to [ TweenDuration duration ] clock
            in
            { gp = cell.gp
            , color = color
            , scaleX = cell.scaleX |> tweenToHelp scaleX
            , scaleY = cell.scaleY |> tweenToHelp scaleY
            , rotation = cell.rotation |> tweenToHelp rotation
            , skewX = cell.skewX |> tweenToHelp skewX
            , skewY = cell.skewY |> tweenToHelp skewY
            }
        )
        |> rAndMap (Random.int 2000 5000)
        |> rAndMap randomColor
        |> rAndMap (randomInInterval ( 0.9, 1.4 ))
        |> rAndMap (randomInInterval ( 0.9, 1.4 ))
        |> rAndMap (randomInInterval ( -pi, pi ))
        |> rAndMap (randomInInterval ( 0, 0.4 ))
        |> rAndMap (randomInInterval ( 0, 0.4 ))



--Random.constant (Cell cell.gp)
--    |> Random.Extra.andMap randomColor
--    |> Random.Extra.andMap
--        (cell.skewX
--            |> tweenTo 15 [ TweenDuration 1800 ] clock
--            |> Random.constant
--        )
--    |> Random.Extra.andMap
--        (cell.skewY
--            |> tweenTo 15 [ TweenDuration 1800 ] clock
--            |> Random.constant
--        )
--    |> Random.Extra.andMap
--        (Random.map2
--            (\r d ->
--                cell.rotation
--                    |> tweenTo r [ TweenDuration d ] clock
--            )
--            (Random.float -pi pi)
--            (Random.int 1000 2000)
--        )
--


randomColor : Generator String
randomColor =
    randomHue |> Random.map (\h -> hsl h 1 0.5)



-- colors = [black, white, orange , red, green, purple, blue]


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
