module RandomWalk exposing (main)

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
    { current : Point
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

        next =
            model.current
                |> movePoint dx dy
                |> clampPointInScreen
    in
    { model
        | current = next
        , seed = seed
        , history =
            ( next, model.current )
                :: model.history
                |> List.take maxSegments
    }


movePoint : Float -> Float -> Point -> Point
movePoint dx dy ( x, y ) =
    ( x + dx, y + dy )


clampPointInScreen : Point -> Point
clampPointInScreen ( x, y ) =
    ( clamp 0 width x
    , clamp 0 height y
    )


type Msg
    = Init Seed
    | OnTick
    | OnClick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init seed ->
            ( { model | seed = seed }, Cmd.none )

        OnTick ->
            ( step model, Cmd.none )

        OnClick ->
            ( { model | current = ( width / 2, height / 2 ), history = [] }, Cmd.none )


view : Model -> Html Msg
view model =
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat width
        , SA.height <| String.fromFloat height
        , SA.stroke "white"
        , SE.onClick OnClick
        ]
        (List.indexedMap
            (\i -> drawSeg (segOpacityAtIndex i))
            model.history
        )


segOpacityAtIndex i =
    1 - toFloat i / maxSegments


drawSeg : Float -> Seg -> Svg msg
drawSeg o ( a, b ) =
    Svg.polyline
        [ TA.points [ a, b ]
        , TA.opacity (TT.Opacity o)
        ]
        []
