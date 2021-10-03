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


type Direction
    = Right
    | Left
    | Up
    | Down


applyN n fn x =
    if n <= 0 then
        x

    else
        applyN (n - 1) fn (fn x)


changeSnakeDirection : Direction -> Model -> Model
changeSnakeDirection nextDirection model =
    if areOpposingDirection nextDirection model.direction then
        model

    else
        { model | nextDirection = nextDirection }


areOpposingDirection : Direction -> Direction -> Bool
areOpposingDirection d1 d2 =
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


step : Model -> Model
step model =
    let
        nextHead =
            model.head
                |> moveGridPointInDirection model.nextDirection
                |> warpGridPoint

        hitsFruit =
            nextHead == model.fruit

        ( nextFruit, nextSeed ) =
            if hitsFruit then
                Random.step randomFruit model.seed

            else
                ( model.fruit, model.seed )
    in
    { model
        | head = nextHead
        , tail =
            model.head
                :: (if hitsFruit then
                        model.tail

                    else
                        model.tail |> dropLast
                   )
        , direction = model.nextDirection
        , fruit = nextFruit
        , seed = nextSeed
    }


randomFruit : Generator GridPoint
randomFruit =
    Random.pair
        (Random.int 0 (gridWidth - 1))
        (Random.int 0 (gridHeight - 1))


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


gridPointToScreenPoint : GridPoint -> Point
gridPointToScreenPoint ( x, y ) =
    ( toFloat x * cellSizeInPx, toFloat y * cellSizeInPx )


type alias Model =
    { seed : Seed
    , head : GridPoint
    , tail : List GridPoint
    , direction : Direction
    , nextDirection : Direction
    , fruit : GridPoint
    , score : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( let
        head =
            ( 0, 10 )
      in
      { head = head
      , tail = List.repeat 10 head
      , direction = Right
      , nextDirection = Right
      , fruit = ( 10, 10 )
      , seed = Random.initialSeed 0
      , score = 0
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
            ( step model, Cmd.none )

        OnKeyNoRepeat key ->
            ( case keyToDirection key of
                Just dir ->
                    changeSnakeDirection dir model

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
        , SA.stroke "none"
        , SA.fill "none"
        ]
        [ viewSnake model
        , viewFruit model.fruit
        ]


viewSnake : Model -> Svg msg
viewSnake model =
    model.head
        :: model.tail
        |> List.map gridPointToScreenPoint
        |> List.map (\( x, y ) -> Svg.circle [ Px.cx x, Px.cy y, Px.r (cellSizeInPx / 2) ] [])
        |> Svg.g [ SA.stroke "none", SA.fill "white" ]


viewFruit : GridPoint -> Svg msg
viewFruit gp =
    let
        ( x, y ) =
            gridPointToScreenPoint gp
    in
    Svg.circle
        [ Px.cx x
        , Px.cy y
        , Px.r (cellSizeInPx / 2)
        , SA.stroke "none"
        , SA.fill "red"
        ]
        []
