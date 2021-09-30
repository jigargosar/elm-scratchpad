module Main exposing (main)

import Browser
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Svg
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Time.every 100 (\_ -> OnTick)
        }


wc =
    { w = 710, h = 400 }


type alias Point =
    ( Float, Float )


type alias Seg =
    ( Point, Point )


type alias Model =
    { current : ( Float, Float )
    , history : List Seg
    , seed : Random.Seed
    }


initialModel : Model
initialModel =
    { current = ( wc.w / 2, wc.h / 2 )
    , history = []
    , seed = Random.initialSeed 0
    }


step : Model -> Model
step model =
    let
        motionGenerator =
            Random.float -6 6

        ( ( dx, dy ), seed ) =
            Random.step
                (Random.pair motionGenerator motionGenerator)
                model.seed

        ( x, y ) =
            model.current

        next =
            ( clamp 0 wc.w (x + dx)
            , clamp 0 wc.h (y + dy)
            )
    in
    { model
        | current = next
        , seed = seed
        , history = ( next, model.current ) :: model.history
    }


makeNMoves : Int -> Model -> Model
makeNMoves n model =
    if n <= 0 then
        model

    else
        makeNMoves (n - 1) (step model)


type Msg
    = OnTick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model |> step, Cmd.none )


view model =
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat wc.w
        , SA.height <| String.fromFloat wc.h
        ]
        [ viewMotion model
        ]


viewMotion model =
    List.indexedMap (\i ( a, b ) -> drawLine_V2 a b 0.5) model.history
        |> Svg.g [ SA.stroke "white" ]


drawLine_V2 a b o =
    Svg.polyline
        [ TA.points [ a, b ]
        , TA.opacity (TT.Opacity o)
        ]
        []
