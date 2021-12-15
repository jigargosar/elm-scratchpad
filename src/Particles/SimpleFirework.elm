module Particles.SimpleFirework exposing (main)

import Time
import Utils exposing (..)


main =
    bElement
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { animNow : Int }


init : () -> ( Model, Cmd Msg )
init () =
    ( { animNow = 0 }, Cmd.none )


type Msg
    = Frame Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 35) (Time.posixToMillis >> Frame)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame animNow ->
            ( { model | animNow = animNow }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        nl =
            secondsToFractionOverNowMills 3 model.animNow
    in
    svg [ viewBoxC 300 300, bgc gray, dBlock, noFill, noStroke ]
        []
