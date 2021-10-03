module SnakeGame exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Generator, Seed)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
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
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}
    , Cmd.none
    )


type Msg
    = OnTick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat width
        , SA.height <| String.fromFloat height
        , SA.stroke "white"
        ]
        []
