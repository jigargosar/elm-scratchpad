module TCBON.MandelbrotSetWebGL exposing (..)

import Browser
import Html.Events
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Math.Vector2 exposing (Vec2, vec2)
import Utils exposing (..)
import WebGL


type alias XYRange =
    { xRange : Float2
    , yRange : Float2
    }


maxT =
    80


resolution =
    400


inputRange : Float2
inputRange =
    ( 0, toFloat resolution )


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


offsetXYDecoder : Decoder Float2
offsetXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "offsetX" JD.float)
        (JD.field "offsetY" JD.float)


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
    = OnCanvasClick Float2


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCanvasClick p ->
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
        [ Html.Lazy.lazy viewMandelGL initialMandelRange
        , Html.Lazy.lazy viewMandelGL model.mandel
        , text "HH"
        ]


viewMandelGL : XYRange -> Html Msg
viewMandelGL mandel =
    let
        factor =
            100

        res =
            resolution * factor
    in
    WebGL.toHtml
        [ haWidth res
        , haHeight res
        , dBlock
        , bgc "pink"
        , style "width" (String.fromFloat (res / factor) ++ "px")
        , style "height" (String.fromFloat (res / factor) ++ "px")
        , Html.Events.on "click" (JD.map OnCanvasClick offsetXYDecoder)
        ]
        [ WebGL.entity vertexShader
            fragmentShader
            mesh
            (let
                ( xMin, xMax ) =
                    mandel.xRange

                ( yMin, yMax ) =
                    mandel.yRange
             in
             { xMin = xMin
             , xMax = xMax
             , yMin = yMin
             , yMax = yMax
             , maxT = maxT
             }
            )
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
    { xMin : Float
    , xMax : Float
    , yMin : Float
    , yMax : Float
    , maxT : Int
    }


vertexShader : WebGL.Shader Vertex Uniforms { v_pos2 : Vec2 }
vertexShader =
    [glsl|
        precision mediump float;
        attribute vec2 position;
        uniform float xMin, xMax, yMin, yMax;
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
            gl_Position = vec4(position.x, -position.y, 0, 1.0);

            float x = rangeMap(-1.0, 1.0, xMin, xMax, position.x);
            float y = rangeMap(-1.0, 1.0, yMin, yMax, position.y);

            v_pos2 = vec2(x,y);
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { v_pos2 : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
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

        vec2 complexSquared(float x, float y){
            return vec2(x * x - y * y, 2.0 * x * y);
        }
        vec2 complexSquared(vec2 c){
            return complexSquared(c.x,c.y);
        }

        const int maxT = 100 ;

        float mandel(vec2 p){
            vec2 t = p;
            for(int i=0; i < maxT; i++ ){
                if(dot(t,t) >= 4.0){
                    return norm(float(maxT), 0.0, float(i));
                }
                t = complexSquared(t) + p;
             }
             return 0.0;
        }



        void main () {
            if (mandel(v_pos2) <= 0.0){
                gl_FragColor = vec4(0.0,0.0,0.0, 1.0);
            }
            else {
                gl_FragColor = vec4(1.0,1.0,1.0, 1.0);
            }
            // float gs =  sqrt(sqrt(mandel(v_pos2)));
            float gs =  (sqrt(mandel(v_pos2)));
            gl_FragColor = vec4(gs, gs, gs, 1.0);
        }
    |]


type alias ComplexNum =
    Float2
