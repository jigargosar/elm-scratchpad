module TCBON.MandelbrotSet exposing (..)

import Browser
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import TypedSvg.Attributes as TA
import Utils exposing (..)



--port sendMandel : ( Int, List Int2 ) -> Cmd msg
--initialCri : CRI
--initialCri =
--    --{ min = vec -2.4 -1.4, max = vec 1.34 1.4 }
--    --{ min = vec -2.2 -1.4, max = vec 0.6 1.4 }
--    --boundsFromWH 0.00035 0.00035 |> centerBoundsAt -0.86192 -0.25289
--    --boundsFromWH 0.001 0.001 |> centerBoundsAt -0.786 -0.16
--    --boundsFromWH 3 2 |> centerBoundsAt -0.8 0
--    --boundsFromWH 0.1 0.1 |> centerBoundsAt -0.815 -0.157
--    --boundsFromWH 0.03 0.03 |> centerBoundsAt -0.815 -0.157
--    --boundsFromWH 0.015 0.015 |> centerBoundsAt -0.797 -0.157
--    criFromCD (vec -0.797 -0.157) 0.015


type alias XYRange =
    { xRange : Float2
    , yRange : Float2
    }


maxT =
    80


resolution =
    100


inputRange : Float2
inputRange =
    ( 0, toFloat resolution )


points =
    rangeWH resolution resolution


mandelViewBox : Attribute a
mandelViewBox =
    let
        w =
            toFloat resolution
    in
    TA.viewBox 0 0 w w


xyRangeFromCD : Vec -> Float -> XYRange
xyRangeFromCD c d =
    let
        ( xRange, yRange ) =
            criFromCD c d
                |> criToXYRanges
    in
    { xRange = xRange
    , yRange = yRange
    }


initialMandelRange : XYRange
initialMandelRange =
    xyRangeFromCD (vec -0.797 -0.157) 0.015


rangeMapInt2ToComplex : XYRange -> Int2 -> ComplexNum
rangeMapInt2ToComplex xyRange =
    toFloat2
        >> mapBoth (rangeMap inputRange xyRange.xRange)
            (rangeMap inputRange xyRange.yRange)


mandelGenerate : XYRange -> List Int2
mandelGenerate xyRange =
    points
        |> List.filter (isInt2MandelMember xyRange)


isInt2MandelMember : XYRange -> Int2 -> Bool
isInt2MandelMember xYRange =
    rangeMapInt2ToComplex xYRange >> belongsToMSet maxT


mandelRender : XYRange -> Html Msg
mandelRender xyRange =
    [ renderDefs
    , mandelGenerate xyRange
        |> List.map renderInt2
        |> group [ pointerEventsNone ]
    ]
        |> Svg.svg
            [ mandelViewBox
            , style "width" (String.fromInt resolution ++ "px")
            , dBlock
            , noFill
            , noStroke
            , overflowHidden
            , style "outline" "auto blue"
            , fill gray
            , Svg.Events.on "click" (JD.map OnSvgClicked svgCoordinateDecoder)
            ]


svgCoordinateDecoder : Decoder Float2
svgCoordinateDecoder =
    JD.map2 Tuple.pair
        (JD.field "offsetX" JD.float)
        (JD.field "offsetY" JD.float)


renderDefs : Svg msg
renderDefs =
    Svg.defs []
        [ Svg.rect
            [ SA.id "unit-rect"
            , SA.width "1"
            , SA.height "1"
            ]
            []
        ]


renderInt2 : Int2 -> Svg msg
renderInt2 ( x, y ) =
    Svg.use
        [ SA.xlinkHref "#unit-rect"
        , SA.x <| String.fromInt x
        , SA.y <| String.fromInt y
        ]
        []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { mandel : XYRange }


init : () -> ( Model, Cmd Msg )
init () =
    ( { mandel = initialMandelRange
      }
    , Cmd.batch
        [--sendMandel ( resolution, mandelGenerate initialMandelRange )
        ]
    )


type Msg
    = OnSvgClicked Float2


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnSvgClicked p ->
            let
                _ =
                    Debug.log "p" p

                c =
                    p
                        |> mapEach round
                        |> rangeMapInt2ToComplex initialMandelRange
                        |> vFromFloat2
            in
            ( { model | mandel = xyRangeFromCD c (0.015 / 2) }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ fontSize "100px" ]
        [ Html.Lazy.lazy mandelRender initialMandelRange
        , Html.Lazy.lazy mandelRender model.mandel
        , text "HH"
        ]


belongsToMSet : Int -> ComplexNum -> Bool
belongsToMSet maxT_ c =
    let
        t0 =
            ( 0, 0 )
    in
    belongsToMSetHelp maxT_ c t0


belongsToMSetHelp : Int -> ComplexNum -> ComplexNum -> Bool
belongsToMSetHelp n c t0 =
    let
        t1 =
            complexSquare t0 |> complexAdd c

        isDiverging =
            complexLengthSquared t1 > 4
    in
    if isDiverging then
        False

    else if n <= 0 then
        not isDiverging

    else
        belongsToMSetHelp (n - 1) c t1


type alias ComplexNum =
    Float2


complexSquare : ComplexNum -> ComplexNum
complexSquare ( a, b ) =
    ( a ^ 2 - b ^ 2, 2 * a * b )


complexAdd : ComplexNum -> ComplexNum -> ComplexNum
complexAdd ( a, b ) ( c, d ) =
    ( a + c, b + d )


complexLengthSquared : ComplexNum -> Float
complexLengthSquared ( a, b ) =
    a ^ 2 + b ^ 2
