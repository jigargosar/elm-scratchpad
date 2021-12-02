module TCBON.MandelbrotSetWebGL exposing (..)

import Browser
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Math.Vector2 exposing (Vec2, vec2)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import TypedSvg.Attributes as TA
import Utils exposing (..)
import WebGL


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
                        |> rangeMapInt2ToComplex initialMandelRange
                        |> vFromFloat2
            in
            ( { model | mandel = xyRangeFromCD c (0.015 / 2) }, Cmd.none )


view : Model -> Html Msg
view _ =
    let
        factor =
            100

        res =
            500 * factor
    in
    WebGL.toHtml
        [ haWidth res
        , haHeight res
        , dBlock
        , bgc "pink"
        , style "width" (String.fromFloat (res / factor) ++ "px")
        , style "height" (String.fromFloat (res / factor) ++ "px")
        ]
        [ WebGL.entity vertexShader fragmentShader mesh {}
        ]



-- MESH


type alias Vertex =
    { position : Vec2
    }


mesh : WebGL.Mesh Vertex
mesh =
    WebGL.triangleStrip
        [ Vertex (vec2 -1 1)
        , Vertex (vec2 -1 -1)
        , Vertex (vec2 1 1)
        , Vertex (vec2 1 -1)
        ]



-- SHADERS


type alias Uniforms =
    {}


vertexShader : WebGL.Shader Vertex Uniforms { v_pos2 : Vec2 }
vertexShader =
    [glsl|
        precision mediump float;
        attribute vec2 position;
        varying vec2 v_pos2;


        float norm(float a, float b, float val){
            return (val - a) / (b - a);
        }

        float lerp(float a, float b, float val){
            return (val * (b - a)) + a;
        }

        float rangeMap(float a, float b, float c, float d, float val){
            return lerp(c, d, (norm(a, b, val)));
        }

        void main () {
            gl_Position = vec4(position, 0, 1.0);

            float x = rangeMap(-1.0, 1.0, -0.5, 0.5, position.x);
            float y = rangeMap(-1.0, 1.0, -0.5, 0.5, position.y);

            v_pos2 = position + vec2(-0.5,0);
            v_pos2 = vec2(x,y);
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { v_pos2 : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec2 v_pos2;

        bool bar(vec2 p){
            vec2 val = p;
            for(int i=0; i<40; i++ ){
                val = vec2(val.x * val.x - val.y * val.y, 2.0 * val.x * val.y ) + p;
            }
            float lenSq = abs(val.x*val.x + val.y*val.y);

            return lenSq < 16.0 ;// || lenSq > 16.0;
        }

        void main () {
            if (bar(v_pos2)){
                gl_FragColor = vec4(0.0,0.0,0.0, 1.0);
            }
            else {
                gl_FragColor = vec4(1.0,1.0,1.0, 1.0);
            }
            // gl_FragColor = vec4(p,1, 1.0);
        }
    |]



--noinspection ElmUnusedSymbol


view1 : Model -> Html Msg
view1 model =
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
