module SnakeGame exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as JD exposing (Decoder)
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes.InPx as Px


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


width =
    500


height =
    500


cellSizeInPx =
    10


gridWidth : Int
gridWidth =
    width // cellSizeInPx


gridHeight : Int
gridHeight =
    height // cellSizeInPx


stepDurationInMillis =
    1000 / stepsPerSecond


stepsPerSecond =
    15


type alias Point =
    ( Float, Float )


type alias GridPoint =
    ( Int, Int )


type alias Snake =
    { head : GridPoint
    , tail : List GridPoint
    , direction : Direction
    , nextDirection : Direction
    }


type Direction
    = Right
    | Left
    | Up
    | Down


emptySnake : Snake
emptySnake =
    { head = ( 0, 10 )
    , tail = []
    , direction = Right
    , nextDirection = Right
    }


initialSnake : Snake
initialSnake =
    emptySnake
        |> applyN 10 moveAndExtendSnake
        |> applyN 1 moveSnake


applyN n fn x =
    if n <= 0 then
        x

    else
        applyN (n - 1) fn (fn x)


changeSnakeDirection : Direction -> Snake -> Snake
changeSnakeDirection nextDirection snake =
    if areOpposite nextDirection snake.direction then
        snake

    else
        { snake | nextDirection = nextDirection }


areOpposite : Direction -> Direction -> Bool
areOpposite d1 d2 =
    let
        getOpposite dir =
            case dir of
                Left ->
                    Right

                Right ->
                    Left

                Up ->
                    Down

                Down ->
                    Up
    in
    d1 == getOpposite d2


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
        nextHead =
            snake.head
                |> moveGridPointInDirection snake.nextDirection
                |> warpGridPoint
    in
    { snake
        | head = nextHead
        , tail = snake.head :: snake.tail |> dropLast
        , direction = snake.nextDirection
    }


warpGridPoint : GridPoint -> GridPoint
warpGridPoint =
    Tuple.mapBoth (modBy gridWidth) (modBy gridHeight)


moveGridPointInDirection : Direction -> GridPoint -> GridPoint
moveGridPointInDirection direction ( x, y ) =
    let
        ( dx, dy ) =
            case direction of
                Right ->
                    ( 1, 0 )

                Left ->
                    ( -1, 0 )

                Up ->
                    ( 0, -1 )

                Down ->
                    ( 0, 1 )
    in
    ( x + dx, y + dy )


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
    ( toFloat x * cellSizeInPx, toFloat y * cellSizeInPx )


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
    | OnKeyNoRepeat String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( { model | snake = moveSnake model.snake }, Cmd.none )

        OnKeyNoRepeat key ->
            ( case keyToDirection key of
                Just dir ->
                    { model | snake = changeSnakeDirection dir model.snake }

                Nothing ->
                    model
            , Cmd.none
            )


keyToDirection : String -> Maybe Direction
keyToDirection key =
    case key of
        "ArrowRight" ->
            Just Right

        "ArrowLeft" ->
            Just Left

        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        _ ->
            Nothing


subscriptions _ =
    Sub.batch
        [ Time.every stepDurationInMillis (\_ -> OnTick)
        , Browser.Events.onKeyDown (JD.map OnKeyNoRepeat noRepeatKeyDecoder)
        ]


noRepeatKeyDecoder : Decoder String
noRepeatKeyDecoder =
    JD.field "repeat" JD.bool
        |> JD.andThen
            (\repeat ->
                if repeat then
                    JD.fail "ignoring repeat key"

                else
                    JD.field "key" JD.string
            )


view : Model -> Html Msg
view model =
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat width
        , SA.height <| String.fromFloat height
        , SA.stroke "white"
        ]
        [ snakeToPoints model.snake
            |> List.map (\( x, y ) -> Svg.circle [ Px.cx x, Px.cy y, Px.r (cellSizeInPx / 2) ] [])
            |> Svg.g [ SA.stroke "none", SA.fill "white" ]
        ]
