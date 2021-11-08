module SlideTileGame exposing (..)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT
import Utils exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


width =
    480


height =
    width


cw =
    50


gpToWorld : GPos -> Vec
gpToWorld =
    vFromGP >> vScale cw >> vAdd1 (cw / 2)


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


view : Model -> Html Msg
view model =
    Svg.svg
        [ saWidth width
        , saHeight height
        , noFill
        , noStroke
        , bgc gray
        ]
        ([ ( 1, ( 0, 0 ) ) ]
            |> List.map viewTile
        )


type alias Tile =
    ( Int, GPos )


viewTile : Tile -> Html Msg
viewTile ( i, gp ) =
    words
        [ fill white
        , xf [ mv (gpToWorld gp) ]
        ]
        (String.fromInt i)
