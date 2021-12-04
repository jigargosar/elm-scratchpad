module TCBON.MandelbrotSetWebGL exposing (..)

import Browser exposing (Document)
import Browser.Events
import Browser.Navigation exposing (Key)
import Html.Attributes as HA
import Html.Events
import Html.Events.Extra.Wheel as Wheel
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Math.Vector2 exposing (Vec2, vec2)
import Url exposing (Url)
import Url.Parser as UrlP
import Url.Parser.Query as Q
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
    {-
       cx: -1.1203830302034128
       cy: -0.2915959449337175
       w: 0.000027272727272727273
       h: 0.000027272727272727273
       zoom: 11000000%
    -}
    --newCRI (vec -0.6 0) (vec 1.5 (1.5 / aspectRatio))
    newCRI (vec -1.1203830302034128 -0.2915959449337175) (vec 1.5 (1.5 / aspectRatio))


canvasCRI : CRI
canvasCRI =
    criFromLTWH 0 0 width height


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , onUrlChange = always NOP
        , onUrlRequest = always NOP
        , update = update
        , view = view
        }


type alias Model =
    { key : Key
    , mandel : CRI
    , drag : Drag
    }


type Drag
    = NotDragging
    | Dragging Vec Vec CRI



--init : () -> Url -> Key -> ( Model, Cmd Msg )


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () _ key =
    let
        p : UrlP.Parser (Maybe Vec -> a) a
        p =
            UrlP.oneOf
                [ UrlP.query
                    (Q.map2 (Maybe.map2 vec)
                        (Q.string "cx" |> Q.map (Maybe.andThen String.toFloat))
                        (Q.string "cy" |> Q.map (Maybe.andThen String.toFloat))
                    )
                ]
    in
    ( { key = key
      , mandel = initialMandelCRI
      , drag = NotDragging
      }
      --|> update (OnCanvasClick ( 30, 157 ))
      --|> update (OnCanvasClick ( 43, 171 ))
      --|> update (OnCanvasClick ( 157, 358 ))
      --|> update (OnCanvasClick ( 111, 313 ))
    , Cmd.none
    )


type Msg
    = NOP
    | OnCanvasMouseDown MouseEvent
    | OnMouseMove MouseEvent
    | OnMouseUp MouseEvent
    | OnCanvasKeyDown KeyEvent
    | OnCanvasWheel Wheel.Event


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Sub.batch
        [ Browser.Events.onAnimationFrame (always NOP)
        , case drag of
            NotDragging ->
                Sub.none

            Dragging _ _ _ ->
                Sub.batch
                    [ Browser.Events.onMouseMove (JD.map OnMouseMove mouseEventDecoder)
                    , Browser.Events.onMouseUp (JD.map OnMouseUp mouseEventDecoder)
                    ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnCanvasKeyDown e ->
            let
                frac =
                    0.05

                mandel =
                    case e.key of
                        "a" ->
                            panByWHFraction ( -frac, 0 ) model.mandel

                        "d" ->
                            panByWHFraction ( frac, 0 ) model.mandel

                        "w" ->
                            panByWHFraction ( 0, -frac ) model.mandel

                        "s" ->
                            panByWHFraction ( 0, frac ) model.mandel

                        "e" ->
                            zoomAroundBy model.mandel.c 0.5 model.mandel

                        "q" ->
                            zoomAroundBy model.mandel.c 2 model.mandel

                        _ ->
                            model.mandel
            in
            ( { model | mandel = mandel }, Cmd.none )

        OnCanvasWheel e ->
            let
                fixedPt =
                    e.mouseEvent.offsetPos
                        |> vFromFloat2
                        |> rangeMapCRI canvasCRI model.mandel

                scale_ =
                    1 + sign e.deltaY * 0.1
            in
            ( { model | mandel = zoomAroundBy fixedPt scale_ model.mandel }, Cmd.none )

        OnCanvasMouseDown e ->
            ( case model.drag of
                NotDragging ->
                    let
                        v =
                            vFromFloat2 e.offset
                    in
                    { model | drag = Dragging v v model.mandel }

                Dragging _ _ _ ->
                    model
            , Cmd.none
            )

        OnMouseMove e ->
            ( case model.drag of
                NotDragging ->
                    model

                Dragging s _ _ ->
                    let
                        end =
                            vFromFloat2 e.offset
                    in
                    { model | drag = Dragging s end (panWithCanvasStartAndEnd s end model.mandel) }
            , Cmd.none
            )

        OnMouseUp _ ->
            ( case model.drag of
                NotDragging ->
                    model

                Dragging _ _ mandel ->
                    { model | drag = NotDragging, mandel = mandel }
            , Cmd.none
            )


zoomAroundBy : Vec -> Float -> CRI -> CRI
zoomAroundBy fixedPt scale_ cri =
    let
        --minScale = 0.5; i=initial,c=current,n=new
        -- iw / nw >= 0.5; i.e >= 50%
        -- iw / (cw * ns) >= 0.5
        -- iw / (cw * 0.5) >= ns
        -- ns <= iw / (cw * 0.5)
        upperBound =
            initialMandelCRI.ri.x / (cri.ri.x * 0.5)

        -- maxScale = 100_000; i=initial,c=current,n=new
        -- iw / nw <= 100_000;
        -- iw / (cw * ns) <= 100_000
        -- iw / (cw * 100_000) <= ns
        -- ns >= iw / (cw * 100_000)
        lowerBound =
            initialMandelCRI.ri.x / (cri.ri.x * (110 * 1000))

        clampedScale =
            scale_
                |> atMost upperBound
                |> atLeast lowerBound
    in
    criZoomByAround fixedPt clampedScale cri


panWithCanvasStartAndEnd : Vec -> Vec -> CRI -> CRI
panWithCanvasStartAndEnd s e cri =
    let
        rm =
            rangeMapCRI canvasCRI cri

        t =
            vFromTo (rm e) (rm s)
    in
    criTranslate t cri


panByWHFraction : Float2 -> CRI -> CRI
panByWHFraction frac2 cri =
    let
        t =
            criDimension cri
                |> map2 mul frac2
                |> vFromFloat2
    in
    criTranslate t cri


view : Model -> Document Msg
view model =
    Document "" [ viewEl model ]


viewEl : Model -> Html Msg
viewEl model =
    let
        mandel =
            case model.drag of
                NotDragging ->
                    model.mandel

                Dragging _ _ mandel_ ->
                    mandel_
    in
    div [ fontSize "30px" ]
        [ stylesNode "html,body{height:100%; background-color:#444;}"
        , Html.Lazy.lazy viewMandelGL mandel
        , div [] [ text ("cx: " ++ String.fromFloat mandel.c.x) ]
        , div [] [ text ("cy: " ++ String.fromFloat mandel.c.y) ]
        , div [] [ text ("w: " ++ String.fromFloat (criWidth mandel)) ]
        , div [] [ text ("h: " ++ String.fromFloat (criHeight mandel)) ]
        , div [] [ text ("zoom: " ++ String.fromInt (mandelZoomPct mandel) ++ "%") ]
        ]


mandelZoomPct : CRI -> Int
mandelZoomPct cri =
    (initialMandelCRI.ri.x / cri.ri.x)
        * 100
        |> round


viewMandelGL : CRI -> Html Msg
viewMandelGL mandel =
    let
        canvasScalingFactor =
            2
    in
    WebGL.toHtml
        [ haWidth (width * canvasScalingFactor)
        , haHeight (height * canvasScalingFactor)
        , dBlock
        , bgc "pink"
        , style "width" (String.fromFloat width ++ "px")
        , style "height" (String.fromFloat height ++ "px")
        , Html.Events.on "mousedown" (JD.map OnCanvasMouseDown mouseEventDecoder)
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

        const int maxT = 50 ;

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
