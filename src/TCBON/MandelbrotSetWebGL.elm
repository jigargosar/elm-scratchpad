module TCBON.MandelbrotSetWebGL exposing (..)

import Browser
import Html.Attributes as HA
import Html.Events
import Html.Events.Extra.Wheel as Wheel
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Math.Vector2 exposing (Vec2, vec2)
import Utils exposing (..)
import WebGL


type alias XYRange =
    { xRange : Float2
    , yRange : Float2
    }


width =
    500


height =
    500


aspectRatio =
    width / height


initialMandelCRI : CRI
initialMandelCRI =
    --criFromCD (vec -0.797 -0.157) 0.015
    newCRI (vec -0.6 0) (vec 1.5 (1.5 / aspectRatio))


canvasCRI : CRI
canvasCRI =
    criFromLTWH 0 0 width height


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { mandel : CRI }


init : () -> ( Model, Cmd Msg )
init () =
    ( { mandel = initialMandelCRI
      }
      --|> update (OnCanvasClick ( 30, 157 ))
      --|> update (OnCanvasClick ( 43, 171 ))
      --|> update (OnCanvasClick ( 157, 358 ))
      --|> update (OnCanvasClick ( 111, 313 ))
    , Cmd.none
    )


type Msg
    = OnCanvasClick MouseEvent
    | OnCanvasKeyDown KeyEvent
    | OnCanvasWheel Wheel.Event


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCanvasClick e ->
            let
                c =
                    e.offset
                        |> Debug.log "p"
                        |> vFromFloat2
                        |> rangeMapCRI canvasCRI model.mandel

                factor =
                    if e.modifiers.shift then
                        2

                    else
                        0.5
            in
            ( { model | mandel = newCRI c (model.mandel.ri |> vScale factor) }, Cmd.none )

        OnCanvasKeyDown e ->
            let
                pct =
                    0.05

                mandel =
                    case e.key of
                        "a" ->
                            criShiftByWHFactor ( -pct, 0 ) model.mandel

                        "d" ->
                            criShiftByWHFactor ( pct, 0 ) model.mandel

                        "w" ->
                            criShiftByWHFactor ( 0, -pct ) model.mandel

                        "s" ->
                            criShiftByWHFactor ( 0, pct ) model.mandel

                        "e" ->
                            criScaleRI 0.5 model.mandel

                        "q" ->
                            criScaleRI 2 model.mandel

                        _ ->
                            model.mandel
            in
            ( { model | mandel = mandel }, Cmd.none )

        OnCanvasWheel e ->
            let
                _ =
                    Debug.log "e" e.deltaY
            in
            ( model, Cmd.none )


criShiftByWHFactor : Float2 -> CRI -> CRI
criShiftByWHFactor ( xf, yf ) cri =
    { cri | c = vAdd cri.c (vec (xf * criWidth cri) (yf * criHeight cri)) }


criScaleRI : Float -> CRI -> CRI
criScaleRI s cri =
    { cri | ri = vScale s cri.ri }


view : Model -> Html Msg
view model =
    div [ fontSize "100px" ]
        [ stylesNode "html,body{height:100%; background-color:#444;}"
        , Html.Lazy.lazy viewMandelGL model.mandel
        ]


viewMandelGL : CRI -> Html Msg
viewMandelGL mandel =
    let
        canvasScalingFactor =
            100
    in
    WebGL.toHtml
        [ haWidth (width * canvasScalingFactor)
        , haHeight (height * canvasScalingFactor)
        , dBlock
        , bgc "pink"
        , style "width" (String.fromInt width ++ "px")
        , style "height" (String.fromInt height ++ "px")
        , Html.Events.on "click" (JD.map OnCanvasClick mouseEventDecoder)
        , Html.Events.on "keydown" (JD.map OnCanvasKeyDown keyEventDecoder)
        , Wheel.onWheel OnCanvasWheel
        , HA.tabindex 0
        , HA.autofocus True
        ]
        [ WebGL.entity vertexShader
            fragmentShader
            mesh
            (let
                ( ( xMin, xMax ), ( yMin, yMax ) ) =
                    criToXYRanges mandel
             in
             { xMin = xMin
             , xMax = xMax
             , yMin = yMin
             , yMax = yMax
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
    }


vertexShader : WebGL.Shader Vertex Uniforms { v_pos2 : Vec2 }
vertexShader =
    [glsl|
        precision highp float;
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
        precision highp float;
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

        const int maxT = 200 ;

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
