module TCBON.MandelbrotSet exposing (..)

import Browser
import Html exposing (Attribute, Html, div)
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import TypedSvg.Attributes as TA
import Utils exposing (..)



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


type alias Mandel =
    { resolution : Int
    , maxT : Int
    , xRange : Float2
    , yRange : Float2
    }


mandelFromCD : Vec -> Float -> Mandel
mandelFromCD c d =
    let
        ( xRange, yRange ) =
            criFromCD c d
                |> criToXYRanges
    in
    { resolution = 250, maxT = 80, xRange = xRange, yRange = yRange }


initialMandel : Mandel
initialMandel =
    mandelFromCD (vec -0.797 -0.157) 0.015


i2ToComplex : Mandel -> Int2 -> ComplexNum
i2ToComplex mandel =
    let
        inputRange : Float2
        inputRange =
            ( 0, toFloat mandel.resolution )
    in
    toFloat2
        >> mapBoth (rangeMap inputRange mandel.xRange)
            (rangeMap inputRange mandel.yRange)


mandelRender : Mandel -> Html Msg
mandelRender mandel =
    let
        renderIfMember : Int2 -> Maybe (Svg msg)
        renderIfMember i2 =
            if belongsToMSet mandel.maxT (i2ToComplex mandel i2) then
                Just (renderInt2 i2)

            else
                Nothing

        mandelViewBox : Attribute a
        mandelViewBox =
            let
                w =
                    toFloat mandel.resolution
            in
            TA.viewBox 0 0 w w
    in
    [ renderDefs
    , rangeWH mandel.resolution mandel.resolution
        |> List.filterMap renderIfMember
        |> group [ style "pointer-events" "none" ]
    ]
        |> Svg.svg
            [ mandelViewBox
            , style "width" (String.fromInt mandel.resolution ++ "px")
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
    { mandel : Mandel }


init : () -> ( Model, Cmd Msg )
init () =
    ( { mandel = initialMandel
      }
    , Cmd.none
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
                        |> i2ToComplex initialMandel
                        |> vFromFloat2
            in
            ( { model | mandel = mandelFromCD c 0.015 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.Lazy.lazy mandelRender initialMandel
        , mandelRender model.mandel
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
