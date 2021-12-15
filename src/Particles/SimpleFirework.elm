module Particles.SimpleFirework exposing (main)

import Ease
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

        ( is, ie ) =
            ( vZero, vec 140 0 )

        ease =
            Ease.inSine

        ( s, e ) =
            ( nl |> ease |> vLerp is ie, nl |> Ease.flip ease |> vLerp is ie )
    in
    svg [ viewBoxC 300 300, bgc gray, dBlock, noFill, noStroke ]
        [ vPolyline [ s, e ] [ strokeW 2, stroke <| hsla 1 1 0.65 1 ] ]
