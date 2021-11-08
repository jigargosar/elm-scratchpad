module SlideTileGame exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}
    , Cmd.none
    )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )


width =
    480


height =
    width


view : Model -> Html Msg
view model =
    Svg.svg
        [ SA.width (String.fromFloat width)
        , SA.height (String.fromFloat height)
        , SA.fill "none"
        , SA.stroke "none"
        , style "background-color" "#333"
        , style "background-color" "#0f0f0f"
        ]
        ([ ( 1, ( 0, 0 ) ) ]
            |> List.map viewTile
        )


type alias Tile =
    ( Int, GPos )


type alias GPos =
    ( Int, Int )


viewTile : Tile -> Html Msg
viewTile ( i, ( gx, gy ) ) =
    Svg.text_
        [ SA.fill "white"
        , style "font-size" "30px"
        , style "transform" "scale(2) translateY(0px)"
        , SA.textAnchor "middle"
        , SA.dominantBaseline "central"
        ]
        [ Svg.text "A AA AA AA AA AA AA AA AA AA A" ]


words : List (Attribute msg) -> String -> Svg msg
words xs str =
    Svg.text_
        (SA.textAnchor "middle"
            :: SA.dominantBaseline "central"
            :: xs
        )
        [ Svg.text str ]
