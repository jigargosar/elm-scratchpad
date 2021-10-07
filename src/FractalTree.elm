module FractalTree exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as JD
import Random exposing (Seed)
import Svg
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Time exposing (Posix)
import TypedSvg.Attributes as TA exposing (points)
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


type alias Model =
    { seed : Seed
    , mouseX : Float
    , updatedAt : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { seed = Random.initialSeed 0
      , mouseX = width / 3
      , updatedAt = 0
      }
    , Cmd.none
    )


type Msg
    = MouseXMoved Float
    | MouseXMovedWithNow Float Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseXMoved x ->
            --( { model | mouseX = x }, Cmd.none )
            ( model, Time.now |> Task.map Time.posixToMillis |> Task.perform (MouseXMovedWithNow x) )

        MouseXMovedWithNow x now ->
            if now - model.updatedAt > 200 then
                ( { model | mouseX = x, updatedAt = now }, Cmd.none )

            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        offsetAngle =
            rangeMap ( 0, width ) ( 0, degrees 90 ) model.mouseX
    in
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat width
        , SA.height <| String.fromFloat height
        , SA.stroke "none"
        , SA.fill "none"
        , SE.on "mousemove"
            (JD.map MouseXMoved
                (JD.field "offsetX" JD.float)
            )
        ]
        [ tree offsetAngle 160
            |> List.map viewSegment
            |> Svg.g [ TA.transform [ TT.Translate (width / 2) height ] ]
        ]


rangeMap ( a, b ) ( c, d ) x =
    norm a b x |> lerp c d


lerp a b x =
    (x * (b - a)) + a


norm a b x =
    (x - a) / (b - a)


viewSegment ( a, b ) =
    Svg.polyline [ points [ a, b ], SA.stroke "white" ]
        []


tree angleOffset branchHeight =
    let
        baseAngle =
            degrees -90

        trunkStart =
            ( 0, 0 )

        trunkEnd =
            moveByRTheta branchHeight baseAngle trunkStart
    in
    ( trunkStart, trunkEnd )
        :: branchHelper
            angleOffset
            (nextBranchHeight branchHeight)
            baseAngle
            trunkEnd
            []
            []


nextBranchHeight branchHeight =
    branchHeight * 0.66


branchHelper angleOffset branchHeight baseAngle rootPoint pending segmentsAcc =
    if branchHeight < 2 then
        case pending of
            [] ->
                segmentsAcc

            ( bh, ba, rp ) :: p ->
                branchHelper angleOffset bh ba rp p segmentsAcc

    else
        let
            ( leftAngle, rightAngle ) =
                ( baseAngle - angleOffset
                , baseAngle + angleOffset
                )

            ( leftEndPoint, rightEndPoint ) =
                ( moveByRTheta branchHeight leftAngle rootPoint
                , moveByRTheta branchHeight rightAngle rootPoint
                )
        in
        branchHelper
            angleOffset
            (nextBranchHeight branchHeight)
            leftAngle
            leftEndPoint
            (( nextBranchHeight branchHeight
             , rightAngle
             , rightEndPoint
             )
                :: pending
            )
            (( rootPoint, leftEndPoint )
                :: ( rootPoint, rightEndPoint )
                :: segmentsAcc
            )


moveByRTheta r theta ( x, y ) =
    let
        ( dx, dy ) =
            fromPolar ( r, theta )
    in
    ( x + dx, y + dy )
