module TCBON.MandelbrotSetWebGL exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Html
import Html.Attributes as HA exposing (href)
import Html.Events
import Html.Events.Extra.Wheel as Wheel
import Html.Lazy
import Json.Decode as JD exposing (Decoder)
import Math.Vector2 exposing (Vec2, vec2)
import Process
import Task
import Url exposing (Url)
import Url.Builder as QB
import Url.Parser as UrlP
import Url.Parser.Query as Q
import Utils exposing (..)
import WebGL


type NavMsg
    = OnUrlChanged Url
    | OnUrlRequest UrlRequest
    | WrapMsg Msg


main : Program () Model NavMsg
main =
    let
        init_ : () -> Url -> Key -> ( Model, Cmd NavMsg )
        init_ a b c =
            init a b c |> mapCmd WrapMsg

        update_ : NavMsg -> Model -> ( Model, Cmd NavMsg )
        update_ navMsg model =
            case navMsg of
                WrapMsg msg ->
                    update msg model
                        |> addEffect (updateUrlEffect model)
                        |> mapCmd WrapMsg

                OnUrlRequest (Internal url) ->
                    if url == model.currentUrl then
                        model |> withNoCmd

                    else
                        ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                OnUrlRequest (External _) ->
                    model |> withNoCmd

                OnUrlChanged url ->
                    updateOnUrlChange url model |> withNoCmd
    in
    Browser.application
        { init = init_
        , subscriptions = subscriptions >> Sub.map WrapMsg
        , onUrlChange = OnUrlChanged
        , onUrlRequest = Debug.log "onUrlRequest" >> OnUrlRequest
        , update = update_
        , view = view >> mapDocument WrapMsg
        }


rxToRIWithAspectRatioOfCri : CRI -> Float -> Vec
rxToRIWithAspectRatioOfCri cri rx =
    vec rx (rx / criAspectRatio cri)


defaultRI : Vec
defaultRI =
    vec 1.5 1.5



--initialMandelCRI : CRI
--initialMandelCRI =
--criFromCD (vec -0.797 -0.157) 0.015
{-
   cx: -1.1203830302034128
   cy: -0.2915959449337175
   w: 0.000027272727272727273
   h: 0.000027272727272727273
   zoom: 11000000%
-}
--newCRI (vec -0.6 0) (vec 1.5 (1.5 / aspectRatio))
--newCRI (vec -1.1203830302034128 -0.2915959449337175) (vec 1.5 (1.5 / criAspectRatio canvasCRI))


type alias Model =
    { key : Key
    , currentUrl : Url
    , mandel : CRI
    , drag : Drag
    , canvas : CRI
    }


type Drag
    = NotDragging
    | Dragging Vec Vec CRI


qFloat str =
    Q.string str |> Q.map (Maybe.andThen String.toFloat)


mandelFromUrl : CRI -> Url -> CRI
mandelFromUrl canvasCRI =
    let
        newMandelCRIFrom_CX_CY_RX : Float -> Float -> Float -> CRI
        newMandelCRIFrom_CX_CY_RX cx cy rx =
            newCRI (vec cx cy) (rxToRIWithAspectRatioOfCri canvasCRI rx)
    in
    let
        maybeMandelFromUrl : Url -> Maybe CRI
        maybeMandelFromUrl url =
            let
                qsCRIParser =
                    UrlP.oneOf
                        [ UrlP.query
                            (Q.map3 (Maybe.map3 newMandelCRIFrom_CX_CY_RX)
                                (qFloat "cx")
                                (qFloat "cy")
                                (qFloat "rx")
                            )
                        ]
            in
            UrlP.parse qsCRIParser { url | path = "" }
                |> Maybe.andThen identity
    in
    \url ->
        maybeMandelFromUrl url
            |> Maybe.withDefault
                (newMandelCRIFrom_CX_CY_RX
                    -1.1203830302034128
                    -0.2915959449337175
                    defaultRI.x
                )


init : () -> Url -> Key -> ( Model, Cmd Msg )
init () url key =
    let
        canvasCRI : CRI
        canvasCRI =
            let
                width =
                    200

                height =
                    500
            in
            criFromLTWH 0 0 width height
    in
    { key = key
    , currentUrl = url
    , mandel = mandelFromUrl canvasCRI url
    , drag = NotDragging
    , canvas = canvasCRI
    }
        --|> withEffect replaceUrlCmd
        |> withCmd
            (Process.sleep 0
                |> Task.andThen (always Browser.Dom.getViewport)
                |> Task.perform
                    (Debug.log "Dom.getViewport"
                        >> .viewport
                        >> criFromViewport
                        >> GotViewportCri
                    )
            )


criFromViewport viewport =
    criFromLTWH 0 0 viewport.width viewport.height


updateUrlEffect : Model -> Model -> Cmd msg
updateUrlEffect oldModel newModel =
    (\_ ->
        if oldModel.mandel /= newModel.mandel then
            Browser.Navigation.pushUrl newModel.key
                (computeCurrentURL newModel)

        else
            Cmd.none
    )
        |> always Cmd.none


computeCurrentURL : Model -> String
computeCurrentURL model =
    let
        { c, ri } =
            model.mandel
    in
    model.currentUrl.path
        ++ QB.toQuery
            [ QB.string "cx" (String.fromFloat c.x)
            , QB.string "cy" (String.fromFloat c.y)
            , QB.string "rx" (String.fromFloat ri.x)
            ]


type Msg
    = NOP
    | GotViewportCri CRI
    | OnCanvasMouseDown MouseEvent
    | OnMouseMove MouseEvent
    | OnMouseUp MouseEvent
    | OnCanvasKeyDown KeyEvent
    | OnSave
    | OnCanvasWheel Wheel.Event


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Sub.batch
        [ case drag of
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
                        |> rangeMapCRI model.canvas model.mandel

                scale_ =
                    1 + sign e.deltaY * 0.1
            in
            ( { model | mandel = zoomAroundBy fixedPt scale_ model.mandel }, Cmd.none )

        OnCanvasMouseDown e ->
            ( case model.drag of
                NotDragging ->
                    let
                        v =
                            vFromFloat2 e.page
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
                            vFromFloat2 e.page
                    in
                    { model | drag = Dragging s end (panWithCanvasStartAndEnd s end model.canvas model.mandel) }
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

        OnSave ->
            if mandelFromUrl model.canvas model.currentUrl == model.mandel then
                model |> withNoCmd

            else
                ( model, Browser.Navigation.pushUrl model.key (computeCurrentURL model) )

        GotViewportCri cri ->
            { model
                | canvas = cri
                , mandel =
                    newCRI model.mandel.c
                        (rxToRIWithAspectRatioOfCri cri model.mandel.ri.x)
            }
                --|> withCmd
                --    (Process.sleep 2000
                --        |> Task.andThen (always Browser.Dom.getViewport)
                --        |> Task.perform (Debug.log "vp" >> .viewport >> criFromViewport >> always NOP)
                --    )
                |> withNoCmd


updateOnUrlChange : Url -> Model -> Model
updateOnUrlChange url model =
    { model | mandel = mandelFromUrl model.canvas url, currentUrl = url }


zoomAroundBy : Vec -> Float -> CRI -> CRI
zoomAroundBy fixedPt scale_ cri =
    let
        --minScale = 0.5; i=initial,c=current,n=new
        -- iw / nw >= 0.5; i.e >= 50%
        -- iw / (cw * ns) >= 0.5
        -- iw / (cw * 0.5) >= ns
        -- ns <= iw / (cw * 0.5)
        upperBound =
            defaultRI.x / (cri.ri.x * 0.5)

        -- maxScale = 100_000; i=initial,c=current,n=new
        -- iw / nw <= 100_000;
        -- iw / (cw * ns) <= 100_000
        -- iw / (cw * 100_000) <= ns
        -- ns >= iw / (cw * 100_000)
        lowerBound =
            defaultRI.x / (cri.ri.x * (110 * 1000))

        -- nw >= mw
        -- cw * ns >= mw
        -- ns >= mw / cw
        --upperBound =
        --    minRI.x / cri.ri.x
        clampedScale =
            scale_
                |> atMost upperBound
                |> atLeast lowerBound
    in
    criZoomByAround fixedPt clampedScale cri


panWithCanvasStartAndEnd : Vec -> Vec -> CRI -> CRI -> CRI
panWithCanvasStartAndEnd s e canvas mandel =
    let
        rm =
            rangeMapCRI canvas mandel

        t =
            vFromTo (rm e) (rm s)
    in
    criTranslate t mandel


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
        [ stylesNode "html,body{height:100%; background-color:#444; overflow:hidden;}"
        , Html.Lazy.lazy2 viewMandelGL model.canvas mandel
        , div [ positionAbsolute, style "width" "100%" ]
            [ Html.a [ style "color" "#FFF", href (computeCurrentURL model) ] [ text "permalink" ]
            , div [] [ text ("cx: " ++ String.fromFloat mandel.c.x) ]
            , div [] [ text ("cy: " ++ String.fromFloat mandel.c.y) ]
            , div [] [ text ("w: " ++ String.fromFloat (criWidth mandel)) ]
            , div [] [ text ("h: " ++ String.fromFloat (criHeight mandel)) ]
            , div [] [ text ("zoom: " ++ String.fromInt (mandelZoomPct mandel) ++ "%") ]
            ]
        ]


mandelZoomPct : CRI -> Int
mandelZoomPct cri =
    (defaultRI.x / cri.ri.x)
        * 100
        |> round


viewMandelGL : CRI -> CRI -> Html Msg
viewMandelGL canvasCRI mandel =
    let
        canvasScalingFactor =
            2
    in
    WebGL.toHtml
        [ attrWidth (criWidth canvasCRI * canvasScalingFactor |> round)
        , attrHeight (criHeight canvasCRI * canvasScalingFactor |> round)
        , dBlock
        , bgc "pink"
        , styleWidth (String.fromFloat (criWidth canvasCRI) ++ "px")
        , styleHeight (String.fromFloat (criHeight canvasCRI) ++ "px")
        , Html.Events.on "mousedown" (JD.map OnCanvasMouseDown mouseEventDecoder)
        , canvasKeyDownAttr
        , Wheel.onWheel OnCanvasWheel
        , HA.tabindex 0
        , HA.autofocus True
        , positionAbsolute
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


canvasKeyDownAttr : Html.Attribute Msg
canvasKeyDownAttr =
    let
        keyMap : List ( KeyEvent -> Bool, KeyEvent -> Msg )
        keyMap =
            [ ( matchesNoModifiers [ "w", "s", "a", "d", "e", "q" ], OnCanvasKeyDown )
            , ( matchesCtrl [ "s", "S" ], always OnSave )
            ]
    in
    Html.Events.preventDefaultOn "keydown" (keyMapDecoderPreventDefault keyMap)



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
