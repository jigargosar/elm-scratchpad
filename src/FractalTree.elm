module FractalTree exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random exposing (Seed)
import Svg
import Svg.Attributes as SA
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
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { seed = Random.initialSeed 0
      }
    , Cmd.none
    )


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view _ =
    let
        _ =
            []
    in
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat width
        , SA.height <| String.fromFloat height
        , SA.stroke "none"
        , SA.fill "none"
        ]
        [ tree (degrees 75) 160
            |> List.map viewSegment
            |> Svg.g [ TA.transform [ TT.Translate (width / 2) height ] ]
        ]


viewSegment ( a, b ) =
    Svg.polyline [ points [ a, b ], SA.stroke "white" ]
        []


tree angleOffset baseBranchHeight =
    let
        trunkEnd =
            moveByRTheta baseBranchHeight (degrees -90) ( 0, 0 )
    in
    ( ( 0, 0 ), trunkEnd )
        :: branchHelper
            angleOffset
            (baseBranchHeight * 0.66)
            (degrees -90)
            trunkEnd
            []
            []


branch angleOffset baseBranchHeight =
    branchHelper angleOffset baseBranchHeight (degrees -90) ( 0, 0 ) [] []


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
            (branchHeight * 0.66)
            leftAngle
            leftEndPoint
            (( branchHeight * 0.66
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
