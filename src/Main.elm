module Main exposing (main)

import Browser
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
import Svg
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Time.every stepRateInMilli (\_ -> OnTick)
        }


width =
    500


height =
    500


maxSegments =
    2000


motionRange =
    6


stepRateInMilli =
    30


type alias Point =
    ( Float, Float )


type alias Seg =
    ( Point, Point )


type alias Model =
    { current : ( Float, Float )
    , history : List Seg
    , seed : Random.Seed
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { current = ( width / 2, height / 2 )
      , history = []
      , seed = Random.initialSeed 0
      }
    , Random.generate Init Random.independentSeed
    )


step : Model -> Model
step model =
    let
        motionGenerator =
            Random.float -motionRange motionRange

        ( ( dx, dy ), seed ) =
            Random.step
                (Random.pair motionGenerator motionGenerator)
                model.seed

        ( x, y ) =
            model.current

        next =
            ( clamp 0 width (x + dx)
            , clamp 0 height (y + dy)
            )
    in
    { model
        | current = next
        , seed = seed
        , history =
            ( next, model.current )
                :: model.history
                |> List.take maxSegments
    }


makeNMoves : Int -> Model -> Model
makeNMoves n model =
    if n <= 0 then
        model

    else
        makeNMoves (n - 1) (step model)


type Msg
    = OnTick
    | Init Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model |> step, Cmd.none )

        Init seed ->
            ( { model | seed = seed }, Cmd.none )


view model =
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat width
        , SA.height <| String.fromFloat height
        , SA.stroke "white"
        ]
        (List.indexedMap
            (\i -> drawSeg (1 - toFloat i / maxSegments))
            model.history
        )


drawSeg o ( a, b ) =
    Svg.polyline
        [ TA.points [ a, b ]
        , TA.opacity (TT.Opacity o)
        ]
        []
