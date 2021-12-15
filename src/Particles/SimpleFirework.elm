module Particles.SimpleFirework exposing (main)

import Browser.Events
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
    [ Time.every (1000 / 35) (Time.posixToMillis >> Frame)
    , Browser.Events.onAnimationFrame (Time.posixToMillis >> Frame)
        |> always Sub.none
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame animNow ->
            ( { model | animNow = animNow }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        nl =
            secondsToFractionOverNowMills 1 model.animNow
    in
    svg [ viewBoxC 300 300, bgc gray, dBlock, noFill, noStroke ]
        [ easeLine nl 1 vZero (vec 140 0) Ease.inSine []
            |> to4
        ]


to4 el =
    List.range 0 3
        |> List.map (\i -> group [ xf [ rotateDeg (toFloat i * 90) ] ] [ el ])
        |> group []


easeLine nl hue is ie ease aa =
    let
        ( s, e ) =
            ( nl |> ease |> vLerp is ie, nl |> Ease.flip ease |> vLerp is ie )
    in
    vPolyline [ s, e ] (strokeW 2 :: stroke (hsla hue 1 0.65 1) :: aa)
