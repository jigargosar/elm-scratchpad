module SnakeGame exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Time.every stepDurationInMillis (\_ -> OnTick)
        }


width =
    500


height =
    500


stepDurationInMillis =
    1000 / stepsPerSecond


stepsPerSecond =
    15


type alias Point =
    ( Float, Float )


type alias Seg =
    ( Point, Point )


type alias Model =
    { seed : Seed
    , snake : Snake
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { seed = Random.initialSeed 0
      , snake = initialSnake
      }
    , Cmd.none
    )


type Msg
    = OnTick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( { model | snake = moveSnake model.snake }, Cmd.none )


view : Model -> Html Msg
view model =
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat width
        , SA.height <| String.fromFloat height
        , SA.stroke "white"
        ]
        [ Svg.polyline
            [ SA.strokeWidth (String.fromFloat cellSize)
            , TA.points (snakeToPoints model.snake)
            ]
            []
        ]


applyN n fn x =
    if n <= 0 then
        x

    else
        applyN (n - 1) fn (fn x)


type alias GridPoint =
    ( Int, Int )


type alias Snake =
    { head : GridPoint
    , tail : List GridPoint
    , direction : Direction
    }


type Direction
    = Right


emptySnake : Snake
emptySnake =
    { head = ( 0, 10 )
    , tail = []
    , direction = Right
    }


initialSnake : Snake
initialSnake =
    emptySnake
        |> applyN 10 moveAndExtendSnake
        |> applyN 1 moveSnake


moveAndExtendSnake : Snake -> Snake
moveAndExtendSnake snake =
    let
        ( hx, hy ) =
            snake.head

        nextHead =
            ( hx + 1, hy )
    in
    { snake | head = nextHead, tail = snake.head :: snake.tail }


moveSnake : Snake -> Snake
moveSnake snake =
    let
        ( hx, hy ) =
            snake.head

        nextHead =
            ( hx + 1, hy )
    in
    { snake | head = nextHead, tail = snake.head :: snake.tail |> dropLast }


dropLast : List a -> List a
dropLast =
    List.reverse >> List.drop 1 >> List.reverse


snakeToPoints : Snake -> List Point
snakeToPoints snake =
    snake.head
        :: snake.tail
        |> List.map gridPointToPoint


gridPointToPoint : GridPoint -> Point
gridPointToPoint ( x, y ) =
    ( toFloat x * cellSize, toFloat y * cellSize )


cellSize =
    10
