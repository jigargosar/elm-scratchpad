module SnakeGame exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Json.Decode as JD exposing (Decoder)
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT


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
    15


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


changeSnakeDirection : Direction -> Model -> Model
changeSnakeDirection nextDirection model =
    if areOpposingDirection nextDirection model.direction then
        model

    else
        { model | requestedDirection = nextDirection }


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
                |> moveGridPointInDirection model.requestedDirection
                |> warpGridPoint
    in
    if listContains nextHead model.tail then
        -- hitsSelf
        { model | gameOver = True }

    else if nextHead == model.fruit then
        -- hits fruit
        let
            ( nextFruit, nextSeed ) =
                Random.step randomFruit model.seed
        in
        { model
            | head = nextHead
            , direction = model.requestedDirection
            , tail = model.head :: model.tail
            , fruit = nextFruit
            , seed = nextSeed
            , score = model.score + 1
        }

    else
        { model
            | head = nextHead
            , direction = model.requestedDirection
            , tail = model.head :: dropLast model.tail
        }


listContains x xs =
    List.any (\y -> y == x) xs


randomFruit : Generator GridPoint
randomFruit =
    let
        offset =
            3
    in
    Random.pair
        (Random.int offset (gridWidth - 1 - offset))
        (Random.int offset (gridHeight - 1 - offset))


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
    , requestedDirection : Direction
    , fruit : GridPoint
    , score : Int
    , gameOver : Bool
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
      , requestedDirection = Right
      , fruit = ( 10, 10 )
      , seed = Random.initialSeed 0
      , score = 0
      , gameOver = False
      }
    , Cmd.none
    )


type Msg
    = OnTick
    | OnKeyDown String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( if not model.gameOver then
                step model

              else
                model
            , Cmd.none
            )

        OnKeyDown key ->
            if model.gameOver && key == " " then
                init ()
                    |> Tuple.mapFirst (\m -> { m | seed = model.seed })

            else
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
        , BE.onKeyDown (JD.map OnKeyDown (JD.field "key" JD.string))
        ]


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
        , viewScore model.score
        , if model.gameOver then
            viewGameOverMessage

          else
            viewEmpty
        ]


viewEmpty =
    text ""


viewGameOverMessage =
    Svg.text_
        [ TA.transform [ TT.Translate (width / 2) (height / 2) ]
        , TA.textAnchor TT.AnchorMiddle
        , TA.dominantBaseline TT.DominantBaselineCentral
        , SA.fill "white"
        ]
        [ Svg.text "Game Over" ]


viewScore score =
    Svg.text_
        [ TA.transform [ TT.Translate 10 10 ]
        , TA.textAnchor TT.AnchorStart
        , TA.dominantBaseline TT.DominantBaselineHanging
        , SA.fill "white"
        ]
        [ Svg.text
            ("Score: " ++ String.fromInt score)
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
