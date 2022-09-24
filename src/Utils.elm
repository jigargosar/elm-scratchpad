module Utils exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Color
import Dict exposing (Dict)
import Ease
import Float.Extra
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events
import Html.Keyed
import Json.Decode as JD exposing (Decoder)
import List.Extra
import Maybe.Extra
import Pivot exposing (Pivot)
import Random exposing (Generator)
import Random.Char
import Random.Extra
import Result.Extra
import Set exposing (Set)
import Svg
import Svg.Attributes as SA
import Svg.Keyed
import Task
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT
import Url exposing (Url)


rangeMap ( a, b ) ( c, d ) x =
    norm a b x |> lerp c d


lerp a b x =
    (x * (b - a)) + a


lerpRange ( a, b ) =
    lerp a b


lerpInt a b =
    lerp (toFloat a) (toFloat b) >> round


norm a b x =
    (x - a) / (b - a)


normClamped a b x =
    norm a b x |> clamp 0 1


type Dir4
    = Up
    | Down
    | Left
    | Right


allDir4 : List Dir4
allDir4 =
    [ Up, Down, Left, Right ]


type alias MouseEvent =
    { modifiers : Modifiers
    , pageXY : Float2
    , clientXY : Float2
    }


type alias CurrentTarget =
    { offsetLeftTop : Float2
    , rootOffsetLeftTop : Float2
    , offsetSize : Float2
    }


mouseEventDecoder : Decoder MouseEvent
mouseEventDecoder =
    JD.succeed MouseEvent
        |> jdAndMap modifiersDecoder
        |> jdAndMap pageXYDecoder
        |> jdAndMap clientXYDecoder


currentTargetDecoder : Decoder CurrentTarget
currentTargetDecoder =
    JD.succeed CurrentTarget
        |> jdAndMap offsetLeftTopDecoder
        |> jdAndMap rootOffsetLeftTopDecoder
        |> jdAndMap offsetSizeDecoder


offsetLeftTopDecoder : Decoder Float2
offsetLeftTopDecoder =
    JD.map2 Tuple.pair
        (JD.field "offsetLeft" JD.float)
        (JD.field "offsetTop" JD.float)


rootOffsetLeftTopDecoder : Decoder Float2
rootOffsetLeftTopDecoder =
    offsetLeftTopDecoder
        |> JD.andThen
            (\xy ->
                JD.field "offsetParent"
                    (JD.nullable (JD.lazy (\_ -> rootOffsetLeftTopDecoder)))
                    |> JD.map (Maybe.map (map2 add xy) >> Maybe.withDefault xy)
            )


type alias KeyEvent =
    { modifiers : Modifiers
    , key : String
    , repeat : Bool
    , targetTagName : String
    , isTargetBodyElement : Bool
    }


onEnter tag =
    Html.Events.on "keydown"
        (keyEventDecoder
            |> JD.andThen
                (\e ->
                    if e.key == "Enter" then
                        JD.succeed tag

                    else
                        JD.fail "not enter"
                )
        )


preventDefaultOnKeyDown =
    Html.Events.preventDefaultOn "keydown"


alwaysPreventDefaultOnKeyDown tag =
    preventDefaultOnKeyDown (JD.succeed ( tag, True ))


onAnimationFrameClampedDelta msg =
    Browser.Events.onAnimationFrameDelta (clamp 1 100 >> msg)


onBrowserKeyDown : (KeyEvent -> msg) -> Sub msg
onBrowserKeyDown msg =
    Browser.Events.onKeyDown (JD.map msg keyEventDecoder)


onBrowserKeyUp msg =
    Browser.Events.onKeyUp (JD.map msg keyDecoder)


keyEventDecoder : Decoder KeyEvent
keyEventDecoder =
    JD.succeed KeyEvent
        |> jdAndMap modifiersDecoder
        |> jdAndMap keyDecoder
        |> jdAndMap repeatDecoder
        |> jdAndMap targetTagNameOrBodyDecoder
        |> jdAndMap isTargetBodyDecoder


isTargetBodyDecoder : Decoder Bool
isTargetBodyDecoder =
    targetTagNameOrBodyDecoder
        |> JD.map (String.toUpper >> eq "BODY")


targetTagNameDecoder : Decoder String
targetTagNameDecoder =
    JD.at [ "target", "tagName" ] JD.string


targetTagNameOrBodyDecoder : Decoder String
targetTagNameOrBodyDecoder =
    JD.oneOf [ targetTagNameDecoder, JD.succeed "BODY" ]


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string


repeatDecoder : Decoder Bool
repeatDecoder =
    JD.field "repeat" JD.bool


keyMapDecoder : List ( KeyEvent -> Bool, KeyEvent -> a ) -> Decoder a
keyMapDecoder keyMap =
    keyEventDecoder
        |> JD.andThen
            (\e ->
                keyMap
                    |> findFirst (\( pred, _ ) -> pred e)
                    |> Maybe.map (\( _, msg ) -> JD.succeed (msg e))
                    |> Maybe.withDefault (JD.fail "")
            )


keyMapDecoderPreventDefault : List ( KeyEvent -> Bool, KeyEvent -> b ) -> Decoder ( b, Bool )
keyMapDecoderPreventDefault keyMap =
    keyMapDecoder keyMap |> JD.map (pairTo True)


type alias Modifiers =
    { shift : Bool
    , ctrl : Bool
    , alt : Bool
    }


noModifiers =
    Modifiers False False False


onlyCtrl =
    Modifiers False True False


onlyAlt =
    Modifiers False False True


onlyCtrlAlt =
    Modifiers False True True


matchesNoModifiers : List String -> KeyEvent -> Bool
matchesNoModifiers keys e =
    List.member e.key keys && (e.modifiers == noModifiers)


matchesCtrl : List String -> KeyEvent -> Bool
matchesCtrl keys e =
    List.member e.key keys && (e.modifiers == onlyCtrl)


matchesAlt : List String -> KeyEvent -> Bool
matchesAlt keys e =
    List.member e.key keys && (e.modifiers == onlyAlt)


matchesCtrlAlt : List String -> KeyEvent -> Bool
matchesCtrlAlt keys e =
    List.member e.key keys && (e.modifiers == onlyCtrlAlt)


modifiersDecoder : Decoder Modifiers
modifiersDecoder =
    let
        bool s =
            jdAndMap (JD.field s JD.bool)
    in
    JD.succeed Modifiers
        |> bool "shiftKey"
        |> bool "ctrlKey"
        |> bool "altKey"


offsetSizeDecoder : Decoder Float2
offsetSizeDecoder =
    JD.map2 Tuple.pair
        (JD.field "offsetWidth" JD.float)
        (JD.field "offsetHeight" JD.float)


pageXYDecoder : Decoder Float2
pageXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "pageX" JD.float)
        (JD.field "pageY" JD.float)


clientXYDecoder : Decoder Float2
clientXYDecoder =
    JD.map2 Tuple.pair
        (JD.field "clientX" JD.float)
        (JD.field "clientY" JD.float)


arrowKeyToDir : String -> Maybe Dir4
arrowKeyToDir key =
    case key of
        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        _ ->
            Nothing


randomDir : Generator Dir4
randomDir =
    Random.uniform Up [ Down, Left, Right ]


oppositeDir4 : Dir4 -> Dir4
oppositeDir4 dir =
    case dir of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


moveInDir4 : Dir4 -> GPos -> GPos
moveInDir4 dir gp =
    let
        offset =
            case dir of
                Up ->
                    ( 0, -1 )

                Down ->
                    ( 0, 1 )

                Left ->
                    ( -1, 0 )

                Right ->
                    ( 1, 0 )
    in
    add2 gp offset


type alias GPos =
    ( Int, Int )


type alias Int2 =
    Num2 Int


type alias Float2 =
    Num2 Float


type alias Num2 number =
    ( number, number )


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


pair =
    Tuple.pair


pairTo : a -> b -> ( b, a )
pairTo b a =
    ( a, b )


sub2 : ( number, number ) -> ( number, number ) -> ( number, number )
sub2 =
    map2 sub


add2 : ( number, number ) -> ( number, number ) -> ( number, number )
add2 =
    map2 add


areAdjacent : GPos -> GPos -> Bool
areAdjacent gp1 gp2 =
    let
        ( x, y ) =
            sub2 gp1 gp2
    in
    abs x + abs y == 1



--noinspection SpellCheckingInspection


manhattenDistance a b =
    let
        ( x, y ) =
            sub2 a b
    in
    abs x + abs y


rangeWH : Int -> Int -> List GPos
rangeWH w h =
    rangeN h
        |> List.concatMap (\y -> rangeN w |> List.map (\x -> ( x, y )))


squareGridPositions : Int -> List GPos
squareGridPositions sz =
    rangeWH sz sz


gpToGridLocal : { a | gridSize : Float, cellSize : Float } -> Int2 -> Float2
gpToGridLocal { gridSize, cellSize } ( x, y ) =
    let
        c0 =
            -(gridSize / 2) + (cellSize / 2)
    in
    ( c0 + toFloat x * cellSize, c0 + toFloat y * cellSize )


rangeN : Int -> List Int
rangeN n =
    List.range 0 (n - 1)


times : Int -> (Int -> b) -> List b
times n fn =
    rangeN n |> List.map fn


type alias IndexLength =
    { index : Int, length : Int }


timesWithIndexAndLength : Int -> (IndexLength -> b) -> List b
timesWithIndexAndLength n fn =
    rangeN n |> List.map (\i -> fn { index = i, length = n })


mapWithIndexAndLength : (IndexLength -> a -> b) -> List a -> List b
mapWithIndexAndLength fn list =
    let
        length =
            List.length list
    in
    List.indexedMap (\i -> fn { index = i, length = length }) list


type alias Vec =
    { x : Float, y : Float }


randomUnitVec : Generator Vec
randomUnitVec =
    Random.map vFromAngle randomAngle


randomVec : Generator Vec
randomVec =
    Random.pair randomNorm randomAngle
        |> Random.map vFromPolar


vFromAngle : Float -> Vec
vFromAngle theta =
    vFromPolar ( 1, theta )


randomAngle : Generator Float
randomAngle =
    Random.map turns randomNorm


randomNorm : Generator Float
randomNorm =
    Random.float 0 1


vFromGP : GPos -> Vec
vFromGP ( gx, gy ) =
    vec (toFloat gx) (toFloat gy)


vec : Float -> Float -> Vec
vec =
    Vec


sampleVecFromTo : Int -> Vec -> Vec -> List Vec
sampleVecFromTo sampleCount s e =
    normSamples sampleCount |> List.map (vLerp s e)


vLerp : Vec -> Vec -> Float -> Vec
vLerp s e n =
    vFromTo s e |> vScale n |> vAdd s


normSamples : Int -> List Float
normSamples ct =
    List.range 1 ct
        |> List.map (toFloat >> norm 1 (toFloat ct))


easeReturn : Ease.Easing -> Ease.Easing
easeReturn e t =
    if t < 0.5 then
        e (t * 2)

    else
        Ease.reverse e ((t - 0.5) * 2)


sqr =
    raiseTo 2


raiseTo b a =
    pow a b


pow a b =
    a ^ b


adjacentUnitVectors : List Vec
adjacentUnitVectors =
    [ vec -1 0
    , vec 1 0
    , vec 0 -1
    , vec 0 1
    ]


vToTuple : Vec -> ( Float, Float )
vToTuple { x, y } =
    ( x, y )


vLen : Vec -> Float
vLen =
    vLenSquared >> sqrt


vLenSquared : Vec -> Float
vLenSquared { x, y } =
    x ^ 2 + y ^ 2


vToPolar : Vec -> ( Float, Float )
vToPolar =
    vToTuple >> toPolar


vFromFloat2 : Float2 -> Vec
vFromFloat2 ( x, y ) =
    vec x y


vFromInt2 : Int2 -> Vec
vFromInt2 =
    mapEach toFloat >> vFromFloat2


vFromPolar : ( Float, Float ) -> Vec
vFromPolar =
    fromPolar >> vFromFloat2


vFromTo : Vec -> Vec -> Vec
vFromTo a b =
    vSub b a


vSub : Vec -> Vec -> Vec
vSub =
    vMap2 sub


vScale : Float -> Vec -> Vec
vScale s { x, y } =
    vec (s * x) (s * y)


vMapEach : (Float -> Float) -> Vec -> Vec
vMapEach fn { x, y } =
    vec (fn x) (fn y)


vAbs : Vec -> Vec
vAbs =
    vMapEach abs


vNegate : Vec -> Vec
vNegate =
    vMapEach negate


vRotate : Float -> Vec -> Vec
vRotate angle =
    vToPolar >> mapSecond (add angle) >> vFromPolar


vSub1 : Float -> Vec -> Vec
vSub1 a { x, y } =
    vec (x - a) (y - a)


vAdd1 : Float -> Vec -> Vec
vAdd1 a { x, y } =
    vec (x + a) (y + a)


vAdd =
    vMap2 add


vZero =
    vec 0 0


vMap2 : (Float -> Float -> Float) -> Vec -> Vec -> Vec
vMap2 fn a b =
    vec (fn a.x b.x) (fn a.y b.y)


vMapR : (Float -> Float) -> Vec -> Vec
vMapR fn =
    vToPolar >> mapFirst fn >> vFromPolar



-- SVG


svg =
    Svg.svg


basicSvg aa =
    basicSvgAttrs aa |> svg


basicSvgAttrs aa =
    dBlock
        :: noFill
        :: noStroke
        :: noUserSelect
        :: bgc gray
        :: aa


group : List (Attribute msg) -> List (Svg msg) -> Svg msg
group =
    Svg.g


keyedGroup : List (Attribute msg) -> List ( String, Svg msg ) -> Svg msg
keyedGroup =
    Svg.Keyed.node "g"


xf =
    TA.transform


mv2 =
    TT.Translate


mvInt2 =
    toFloat2 >> mvT


mvT ( x, y ) =
    mv2 x y


mvUp y =
    mv2 0 -y


mvRight x =
    mv2 x 0


mvLeft x =
    mvRight -x


scale s =
    TT.Scale s s


rotateDeg deg =
    TT.Rotate deg 0 0


mv { x, y } =
    mv2 x y


hsl : Float -> Float -> Float -> String
hsl h s l =
    Color.hsl h s l |> Color.toCssString


hsla : Float -> Float -> Float -> Float -> String
hsla h s l a =
    Color.hsla h s l a |> Color.toCssString


whiteA : Float -> String
whiteA =
    hsla 1 1 1


blackA : Float -> String
blackA =
    hsla 0 0 0


black =
    grayN 0.1


gray =
    grayN 0.2


white =
    grayN 1


grayN : Float -> String
grayN n =
    rgb n n n


green =
    hsl 0.42 1 0.5


rgb : Float -> Float -> Float -> String
rgb r g b =
    Color.rgb r g b |> Color.toCssString


type alias TRBL =
    { top : Float
    , right : Float
    , bottom : Float
    , left : Float
    }


borderRadius =
    style "border-radius"


borderRadius50 =
    borderRadius "50%"


borderNone =
    style "border" "none"


sOutline =
    style "outline"


outlineNone =
    sOutline "none"


bgc =
    style "background-color"


bgcInherit =
    bgc "inherit"


noFill =
    fill "none"


transparent =
    "transparent"


bgcTransparent =
    bgc transparent


fillTransparent =
    fill transparent


noStroke =
    stroke "none"


aWidth =
    SA.width


aWidthF =
    fromFloat >> aWidth


aHeight =
    SA.height


aHeightF =
    fromFloat >> aHeight


attrX =
    SA.x


attrXF =
    fromFloat >> attrX


attrY =
    SA.y


attrYF =
    fromFloat >> attrY


saWidth =
    SA.width << String.fromFloat


saHeight =
    SA.height << String.fromFloat


strokeW =
    SA.strokeWidth << String.fromFloat


strokeCapRound =
    TA.strokeLinecap TT.StrokeLinecapRound


fill =
    SA.fill


stroke =
    SA.stroke


withTitle : String -> List (Html msg) -> Document msg
withTitle title body =
    Document title body


styleNode : String -> Html msg
styleNode string =
    Html.node "style" [] [ Html.text string ]


textDecoration =
    style "text-decoration"


whiteSpace =
    style "white-space"


ttu =
    textTransform "uppercase"


ttInherit =
    textTransform "inherit"


textTransform =
    style "text-transform"


fg =
    style "color"


fgInherit =
    fg "inherit"


styleAccentColor =
    style "accent-color"


fgCurrentColor =
    fg "currentColor"


bgCurrentColor =
    bgc "currentColor"


animateCssNode =
    styleNode """
@import url('https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css');
"""


cnAnimated =
    "animate__animated"


cnHeadShake =
    "animate__headShake"


cnZoomIn =
    "animate__zoomIn"


cnSlideOutLeft =
    "animate__slideOutLeft"


cnSlideOutUp =
    "animate__slideOutUp"


cnSlideInRight =
    "animate__slideInRight"


cnFaster =
    "animate__faster"


cnSlow =
    "animate__slow"


cnSlower =
    "animate__slower"


cnDelay1s =
    "animate__delay-1s"


basicStylesNode : Html msg
basicStylesNode =
    styleNode """
@import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');

*, *::before, *::after {
    box-sizing: border-box;
    margin:0;
    padding:0;
}

html, body {
    height:100%;
    display:grid;
}

body {
    color: #fafafa;
    background-color:#222;
    font-size: 20px;
    font-family:-apple-system, BlinkMacSystemFont, Segoe UI, Roboto, Helvetica, Arial, sans-serif;
}
"""


overflowVisible =
    style "overflow" "visible"


overflow =
    style "overflow"


overflowClip =
    overflow "clip"


overflowHidden =
    style "overflow" "hidden"


fontInherit =
    style "font" "inherit"


fontSize =
    style "font-size"


fontSizeInherit =
    fontSize "inherit"


fontFamilyInherit =
    fontFamily "inherit"


positionAbsolute =
    style "position" "absolute"


absoluteFill =
    inset0


inset0 =
    style "inset" "0"


positionFixed =
    style "position" "fixed"


cursorPointer =
    style "cursor" "pointer"


cursorGrab =
    style "cursor" "grab"


cursorGrabbing =
    style "cursor" "grabbing"


cursorText =
    style "cursor" "text"


cursorInherit =
    style "cursor" "inherit"


userSelectText =
    style "user-select" "text"


positionRelative =
    style "position" "relative"


backgroundImages ls =
    style "background-image" (ls |> String.join ",")


styleWidth =
    style "width"


styleHeight =
    style "height"


styleLineHeight =
    style "line-height"


styleWidthFPx =
    styleWidth << fpx


styleHeightFPx =
    styleHeight << fpx


styleWidthIPx =
    styleWidth << ipx


styleHeightIPx =
    styleHeight << ipx


fpx : Float -> String
fpx n =
    fromFloat n ++ "px"


fDeg : Float -> String
fDeg n =
    fromFloat n ++ "deg"


fRad : Float -> String
fRad n =
    fromFloat n ++ "rad"


fTurn : Float -> String
fTurn n =
    fromFloat n ++ "turn"


ipx : Int -> String
ipx n =
    fromInt n ++ "px"


fromFloat =
    String.fromFloat


fromInt =
    String.fromInt


transitionTransform =
    style "transition" "transform 300ms"


transitionOpacity =
    style "transition" "opacity 300ms"


transitionFG =
    style "transition" "color 300ms"


transitionFill =
    style "transition" "fill 300ms"


ffMonospace =
    fontFamily "monospace"


fontFamily =
    style "font-family"


style : String -> String -> Attribute msg
style =
    HA.style


attrHeightI =
    HA.height


attrWidthI =
    HA.width


tabindex : Int -> Attribute msg
tabindex =
    HA.tabindex


autofocus : Bool -> Attribute msg
autofocus =
    HA.autofocus


allPointerEvents =
    pointerEvents "all"


noPointerEvents =
    pointerEvents "none"


pointerEventsFromBool bool =
    if bool then
        pointerEvents "auto"

    else
        pointerEvents "none"


pointerEvents =
    style "pointer-events"


tac =
    style "text-align" "center"


bold =
    fontWeight "bold"


fontWeight =
    style "font-weight"


gap =
    style "gap"


h100 =
    sHeight "100%"


w100 =
    sWidth "100%"


sHeight =
    style "height"


sWidth =
    style "width"


sMaxHeight =
    style "max-height"


sMaxWidth =
    style "max-width"


sMinHeight =
    style "min-height"


sMinWidth =
    style "min-width"


aspectRatio =
    style "aspect-ratio"


placeContentCenter =
    style "place-content" "center"


placeItemsCenter =
    style "place-items" "center"


contentCenter =
    justifyContent "center"


justifyContent =
    style "justify-content"


itemsCenter =
    style "align-items" "center"


pa =
    style "padding"


paddingXY x y =
    pa (y ++ " " ++ x)


pl =
    style "padding-left"


pt =
    style "padding-top"


pr =
    style "padding-right"


pb =
    style "padding-bottom"


ma =
    style "margin"


margin0 =
    ma "0"


transforms =
    String.join " " >> style "transform"


translateF2 ( x, y ) =
    "translate(" ++ fpx x ++ "," ++ fpx y ++ ")"


translateT ( a, b ) =
    "translate(" ++ a ++ "," ++ b ++ ")"


rotateF f =
    "rotate(" ++ fromFloat f ++ "rad)"


scaleF f =
    "scale(" ++ fromFloat f ++ ")"


scaleY f =
    "scaleY(" ++ fromFloat f ++ ")"


paf =
    fpx >> pa


displayFlex =
    dFlex


dFlex =
    style "display" "flex"


displayInlineBlock =
    style "display" "inline-block"


dBlock =
    style "display" "block"


displayGrid =
    style "display" "grid"


gridAreaXY : ( Int, Int ) -> Attribute msg
gridAreaXY ( c, r ) =
    style "grid-area" (fromInt (r + 1) ++ "/" ++ fromInt (c + 1))


gridTemplate =
    style "grid-template"


gridTemplateRows =
    style "grid-template-rows"


gridTemplateColumns =
    style "grid-template-columns"


gridAutoColumns =
    style "grid-auto-columns"


gridArea : String -> Attribute msg
gridArea =
    style "grid-area"


gridAutoFlowColumn =
    style "grid-auto-flow" "column"


gridAutoFlowRow =
    style "grid-auto-flow" "row"


flexColumn =
    style "flex-direction" "column"


flexRow =
    style "flex-direction" "row"


fDCol =
    flexColumn


noUserSelect =
    style "user-select" "none"


type alias Html msg =
    Html.Html msg


{-| This data specifies the `<title>` and all of the nodes that should go in
the `<body>`. This means you can update the title as your application changes.
Maybe your "single-page app" navigates to a "different page", maybe a calendar
app shows an accurate date in the title, etc.

> **Note about CSS:** This looks similar to an `<html>` document, but this is
> not the place to manage CSS assets. If you want to work with CSS, there are
> a couple ways:
>
> 1.  Packages like [`rtfeldman/elm-css`][elm-css] give all of the features
>     of CSS without any CSS files. You can add all the styles you need in your
>     `view` function, and there is no need to worry about class names matching.
>
> 2.  Compile your Elm code to JavaScript with `elm make --output=elm.js` and
>     then make your own HTML file that loads `elm.js` and the CSS file you want.
>     With this approach, it does not matter where the CSS comes from. Write it
>     by hand. Generate it. Whatever you want to do.
>
> 3.  If you need to change `<link>` tags dynamically, you can send messages
>     out a port to do it in JavaScript.
>
> The bigger point here is that loading assets involves touching the `<head>`
> as an implementation detail of browsers, but that does not mean it should be
> the responsibility of the `view` function in Elm. So we do it differently!

[elm-css]: /packages/rtfeldman/elm-css/latest/

-}
type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type alias Attribute msg =
    Html.Attribute msg


type alias Svg a =
    Svg.Svg a


type alias Transform =
    TT.Transform


text : String -> Html msg
text =
    Html.text


div : List (Attribute msg) -> List (Html msg) -> Html msg
div =
    Html.div


span : List (Attribute msg) -> List (Html msg) -> Html msg
span =
    Html.span


button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    Html.button


fRow : List (Attribute msg) -> List (Html msg) -> Html msg
fRow aa =
    div (dFlex :: aa)


fCol : List (Attribute msg) -> List (Html msg) -> Html msg
fCol aa =
    div (dFlex :: fDCol :: aa)


gtCols : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
gtCols i aa =
    div (displayGrid :: gridCols i :: aa)


gtRows : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
gtRows i aa =
    div (displayGrid :: gridRows i :: aa)


gridCols : Int -> Attribute msg
gridCols i =
    gridTemplateColumns ("repeat(" ++ fromInt i ++ ", minmax(0,1fr) )")


gridRows : Int -> Attribute msg
gridRows i =
    gridTemplateRows ("repeat(" ++ fromInt i ++ ", minmax(0,1fr) )")


circle : Float -> List (Attribute msg) -> Svg msg
circle r xs =
    Svg.circle (Px.r r :: xs) []


ngonVertices : Int -> Float -> List Vec
ngonVertices n r =
    ngonVerticesHelper 0 n r []


ngonVerticesHelper : Int -> Int -> Float -> List Vec -> List Vec
ngonVerticesHelper i n radius acc =
    if i > n then
        acc

    else
        let
            a =
                turns (toFloat i / toFloat n)

            x =
                radius * cos a

            y =
                radius * sin a
        in
        ngonVerticesHelper (i + 1) n radius (acc ++ [ vec x y ])


nestedSvg : Float -> Float -> List (Attribute msg) -> List (Svg msg) -> Html msg
nestedSvg w h xs =
    Svg.svg
        (saWidth w
            :: saHeight h
            :: Px.x (w / -2)
            :: Px.y (h / -2)
            :: xs
        )


rect : Float -> Float -> List (Attribute msg) -> Svg msg
rect w h aa =
    rectLT w h (Px.x (w / -2) :: Px.y (h / -2) :: aa)


rectT w h aa =
    rectLT w h (Px.x (w / -2) :: aa)


rectB w h aa =
    rectLT w h (Px.x (w / -2) :: Px.y -h :: aa)


rectLT : Float -> Float -> List (Attribute msg) -> Svg msg
rectLT w h aa =
    Svg.rect
        (saWidth w
            :: saHeight h
            :: aa
        )
        []


square : Float -> List (Attribute msg) -> Svg msg
square sz =
    rect sz sz


squareT s =
    rectT s s


squareLT : Float -> List (Attribute msg) -> Svg msg
squareLT sz =
    rectLT sz sz


vPolyline : List Vec -> List (Attribute msg) -> Svg msg
vPolyline vs =
    polyline (List.map vToTuple vs)


polyline : List Float2 -> List (Attribute msg) -> Svg msg
polyline pts aa =
    Svg.polyline (TA.points pts :: aa) []


rotateTurns =
    rotateF << turns


triangle r =
    vPolygon (ngonVertices 3 r)


vPolygon : List Vec -> List (Attribute msg) -> Svg.Svg msg
vPolygon vs =
    polygon (List.map vToTuple vs)


polygon : List Float2 -> List (Attribute msg) -> Svg.Svg msg
polygon pts attrs =
    Svg.polygon (TA.points pts :: attrs) []


viewBoxC : Float -> Float -> Attribute a
viewBoxC w h =
    TA.viewBox (-w / 2) (-h / 2) w h


viewBoxFromScreen : Screen -> Attribute a
viewBoxFromScreen screen =
    viewBoxC screen.width screen.height


viewBoxLT w h =
    TA.viewBox 0 0 w h


wordsAlignXLeft =
    TA.textAnchor TT.AnchorStart


wordsAlignXRight =
    TA.textAnchor TT.AnchorEnd


wordsAlignYTop =
    TA.dominantBaseline TT.DominantBaselineHanging


words : String -> List (Svg.Attribute msg) -> Svg msg
words str xs =
    Svg.text_
        (SA.textAnchor "middle"
            :: SA.dominantBaseline "central"
            :: xs
        )
        [ Svg.text str ]



-- BASICS


isBlank =
    String.trim >> String.isEmpty


nbsp : String
nbsp =
    "\u{00A0}"


fdiv =
    (/)


mul =
    (*)


eq =
    (==)


propEq extract v from =
    extract from == v


eqBy fn a b =
    fn a == fn b


neq =
    (/=)


inc =
    add 1


dec =
    add -1


add =
    (+)


sub =
    (-)


roundFloat =
    round >> toFloat


maxInt =
    Random.maxInt


minInt =
    Random.minInt


isEven : Int -> Bool
isEven x =
    modBy 2 x == 0


isOdd : Int -> Bool
isOdd =
    isEven >> not


signum : number -> number
signum x =
    case compare x 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


isBounded : comparable -> comparable -> comparable -> Bool
isBounded lo hi x =
    not (x < lo || x > hi)



-- TUPLE UTILS


first =
    Tuple.first


second =
    Tuple.second


mapSecond =
    Tuple.mapSecond


mapFirst =
    Tuple.mapFirst


mapBoth =
    Tuple.mapBoth


biMap =
    Tuple.mapBoth


swap ( a, b ) =
    ( b, a )


mapEach : (a -> x) -> ( a, a ) -> ( x, x )
mapEach fn =
    mapBoth fn fn


filterMapSecond : (b -> Maybe c) -> ( a, b ) -> Maybe ( a, c )
filterMapSecond fn ( a, b ) =
    fn b |> Maybe.map (pair a)


toFloat2 : Int2 -> Float2
toFloat2 =
    mapEach toFloat


withRollback : (a -> Maybe a) -> a -> a
withRollback fn x =
    case fn x of
        Nothing ->
            x

        Just y ->
            y


applyN : Int -> (a -> a) -> a -> a
applyN n fn x =
    if n <= 0 then
        x

    else
        applyN (n - 1) fn (fn x)


scanApplyN n fn x =
    scanApplyNHelp n fn x [] |> List.reverse


scanApplyNHelp n fn x acc =
    if n <= 0 then
        x :: acc

    else
        scanApplyNHelp (n - 1) fn (fn x) (x :: acc)


sumBy : (a -> number) -> List a -> number
sumBy fn =
    List.foldl (fn >> add) 0


maxBy : (a -> number) -> List a -> number
maxBy fn =
    List.foldl (fn >> max) 0


atLeast =
    max


atMost =
    min


secondsToFractionOverNowMills : Float -> Int -> Float
secondsToFractionOverNowMills periodSec nowMS =
    let
        pMS =
            periodSec * 1000
    in
    toFloat (modBy (round pMS) nowMS) / pMS


nextListItemEvery : Float -> List a -> Int -> Maybe a
nextListItemEvery itemPeriodInSec list t =
    if list == [] then
        Nothing

    else
        let
            len =
                List.length list |> toFloat

            periodInSec =
                itemPeriodInSec * len

            idx =
                floor (secondsToFractionOverNowMills periodInSec t * len)
        in
        listGetAt idx list


nextNonEmptyListItemEvery : Float -> ( a, List a ) -> Int -> a
nextNonEmptyListItemEvery itemPeriodInSec ( head, tail ) t =
    nextListItemEvery itemPeriodInSec (head :: tail) t
        |> Maybe.withDefault head


listGetAt : Int -> List a -> Maybe a
listGetAt idx =
    List.drop idx >> List.head


listGetAtOrDefault fallback idx =
    listGetAtOr idx fallback


listGetAtOr idx fallback =
    listGetAt idx >> Maybe.withDefault fallback


minBy : (a -> comparable) -> a -> a -> a
minBy fn a b =
    if fn a < fn b then
        a

    else
        b


insertBy : (v -> comparable) -> v -> Dict comparable v -> Dict comparable v
insertBy keyFn v =
    Dict.insert (keyFn v) v


dictBy : (v -> comparable) -> List v -> Dict comparable v
dictBy keyFn =
    List.foldl (insertBy keyFn) Dict.empty



-- CRI


type alias CRI =
    { c : Vec, ri : Vec }


type alias Bounds =
    { min : Vec, max : Vec }


newCRI : Vec -> Vec -> CRI
newCRI c ri =
    { c = c, ri = ri }


criFromLTWH : Float -> Float -> Float -> Float -> CRI
criFromLTWH l t w h =
    let
        ri =
            vec w h |> vScale 0.5

        c =
            vec l t |> vAdd ri
    in
    newCRI c ri


criFromCR : Vec -> Float -> CRI
criFromCR c r =
    newCRI c (vec r r)


criFromCD : Vec -> Float -> CRI
criFromCD c d =
    criFromCR c (d / 2)


criFromD : Float -> CRI
criFromD d =
    criFromR (d / 2)


criFromR : Float -> CRI
criFromR r =
    criFromCR vZero r


criToXYRanges : CRI -> ( Float2, Float2 )
criToXYRanges =
    criToBounds >> boundsToXYRanges


criZoomByAround : Vec -> Float -> CRI -> CRI
criZoomByAround fixedPt scale_ { c, ri } =
    let
        v1 =
            vFromTo c fixedPt

        v2 =
            vScale scale_ v1
    in
    newCRI (vSub c (vFromTo v1 v2))
        (ri |> vScale scale_)


criTranslate : Vec -> CRI -> CRI
criTranslate t { c, ri } =
    newCRI (vAdd c t) ri


rangeMapCRI : CRI -> CRI -> Vec -> Vec
rangeMapCRI inCRI outCRI { x, y } =
    let
        ( inX, inY ) =
            criToXYRanges inCRI

        ( outX, outY ) =
            criToXYRanges outCRI
    in
    vec (rangeMap inX outX x) (rangeMap inY outY y)


boundsToXYRanges : Bounds -> ( Float2, Float2 )
boundsToXYRanges { min, max } =
    ( ( min.x, max.x ), ( min.y, max.y ) )


criToBounds : CRI -> Bounds
criToBounds cri =
    { min = criToMin cri, max = criToMax cri }


criAspectRatio : CRI -> Float
criAspectRatio { ri } =
    ri.x / ri.y


criToWH : CRI -> Float2
criToWH cri =
    ( criWidth cri, criHeight cri )


criDimension : CRI -> Float2
criDimension =
    criToWH


criWidth =
    .ri >> .x >> mul 2


criHeight =
    .ri >> .y >> mul 2


criToMin : CRI -> Vec
criToMin { c, ri } =
    vAdd c (vNegate ri)


criToMax : CRI -> Vec
criToMax { c, ri } =
    vAdd c ri


criToViewBox : CRI -> Attribute a
criToViewBox cri =
    let
        { x, y } =
            criToMin cri

        ( w, h ) =
            criToWH cri
    in
    TA.viewBox x y w h


criToPointsWithXStep : Int -> CRI -> List Float2
criToPointsWithXStep intXSteps cri =
    let
        xSteps : Float
        xSteps =
            toFloat intXSteps

        { min, max } =
            criToBounds cri

        xs =
            Float.Extra.range { start = min.x, end = max.x, steps = round xSteps }

        ySteps : Float
        ySteps =
            xSteps / criAspectRatio cri

        ys =
            Float.Extra.range { start = min.y, end = max.y, steps = round ySteps }
    in
    ys
        |> List.map (\y -> xs |> List.map (\x -> ( x, y )))
        |> List.concat


criScaleRI : Float -> CRI -> CRI
criScaleRI s cri =
    { cri | ri = vScale s cri.ri }



-- UPDATE HELPERS


withNoCmd : model -> ( model, Cmd msg )
withNoCmd =
    pairTo Cmd.none


withCmd : Cmd msg -> model -> ( model, Cmd msg )
withCmd cmd model =
    ( model, cmd )


withEffect : (model -> Cmd msg) -> model -> ( model, Cmd msg )
withEffect effect model =
    ( model, effect model )


addEffect : (model -> Cmd msg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
addEffect effect ( model, cmd ) =
    ( model, Cmd.batch [ effect model, cmd ] )


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmd2 ( model, cmd1 ) =
    ( model, Cmd.batch [ cmd1, cmd2 ] )


mapCmd : (msg1 -> msg2) -> ( model, Cmd msg1 ) -> ( model, Cmd msg2 )
mapCmd tagger =
    mapSecond (Cmd.map tagger)


mapDocument : (msg1 -> msg2) -> Document msg1 -> Document msg2
mapDocument tagger { title, body } =
    Document title (List.map (Html.map tagger) body)



-- DICT HELPERS


replaceEntry : ( comparable, b ) -> Dict comparable b -> Dict comparable b
replaceEntry ( k, v ) =
    Dict.update k (Maybe.map (always v))


replaceEntries : List ( comparable, b ) -> Dict comparable b -> Dict comparable b
replaceEntries entries dict =
    List.foldl replaceEntry dict entries


{-| Only when `from` is member and `to` is not member.
-}
moveValueFromKeyToKey :
    comparable
    -> comparable
    -> Dict comparable v
    -> Dict comparable v
moveValueFromKeyToKey from to dict =
    case ( Dict.get from dict, Dict.get to dict ) of
        ( Just value, Nothing ) ->
            dict
                |> Dict.remove from
                |> Dict.insert to value

        _ ->
            dict


mapValueAndSwapWithKey : (v -> comparable) -> Dict k v -> Dict comparable k
mapValueAndSwapWithKey fn =
    Dict.foldl (\k v -> Dict.insert (fn v) k) Dict.empty


dictKeepKeys : Set comparable -> Dict comparable x -> Dict comparable x
dictKeepKeys keys =
    filterKey (\k -> Set.member k keys)


filterKey : (comparable -> Bool) -> Dict comparable b -> Dict comparable b
filterKey fn =
    Dict.filter (\k _ -> fn k)


renameKey : (a -> comparable) -> Dict a v -> Dict comparable v
renameKey fn =
    Dict.toList >> List.map (Tuple.mapFirst fn) >> Dict.fromList


dictGetOr : a -> comparable -> Dict comparable a -> a
dictGetOr default key dict =
    Dict.get key dict |> Maybe.withDefault default


getInDict : Dict comparable v -> comparable -> Maybe v
getInDict dict key =
    Dict.get key dict


getEntry : comparable -> Dict comparable a -> Maybe ( comparable, a )
getEntry key dict =
    Dict.get key dict |> Maybe.map (pair key)


getEntryIn : Dict comparable a -> comparable -> Maybe ( comparable, a )
getEntryIn dict key =
    getEntry key dict


{-| Name inspired from Maybe.Extra.traverse

Like [`combine`](#combine), but map a function over each element of the list first.

If every function call succeeds (returns `Just`), `traverse` will return a list.
If any function call fails (returns `Nothing`), `traverse` will return `Nothing`.

`combine` is equivalent to `traverse identity`.

    traverse (\x -> Just (x * 10)) [ 1, 2, 3, 4, 5 ]
    --> Just [ 10, 20, 30, 40, 50 ]

    traverse List.head [ [1], [2, 3], [] ]
    --> Nothing

-}
maybeTraverseValues :
    (a -> Maybe b)
    -> Dict comparable a
    -> Maybe (Dict comparable b)
maybeTraverseValues fn =
    let
        reduce :
            Dict comparable b
            -> List ( comparable, a )
            -> Maybe (Dict comparable b)
        reduce dict list =
            case list of
                [] ->
                    Just dict

                ( k, v ) :: t ->
                    case fn v of
                        Nothing ->
                            Nothing

                        Just nv ->
                            reduce (Dict.insert k nv dict) t
    in
    Dict.toList >> reduce Dict.empty



-- LIST HELPERS


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 trips =
    let
        step ( x, y, z ) ( xs, ys, zs ) =
            ( cons x xs, cons y ys, cons z zs )
    in
    List.foldr step ( [], [], [] ) trips


applyAll : List (a -> a) -> a -> a
applyAll fns a =
    List.foldl (<|) a fns


mapHead : (a -> a) -> List a -> List a
mapHead fn xs =
    case xs of
        [] ->
            []

        h :: t ->
            fn h :: t



--noinspection SpellCheckingInspection


filterMapAccuml :
    (acc -> a -> ( acc, Maybe b ))
    -> acc
    -> List a
    -> ( acc, List b )
filterMapAccuml f acc0 list =
    let
        ( accFinal, generatedList ) =
            List.foldl
                (\x ( acc1, ys ) ->
                    let
                        ( acc2, y ) =
                            f acc1 x
                    in
                    ( acc2, y :: ys )
                )
                ( acc0, [] )
                list
    in
    ( accFinal, List.foldl Maybe.Extra.cons [] generatedList )


keep : (a -> Bool) -> List a -> List a
keep =
    List.filter


reject : (a -> Bool) -> List a -> List a
reject fn =
    keep (fn >> not)


uncons : List a -> Maybe ( a, List a )
uncons xs =
    case xs of
        h :: t ->
            Just ( h, t )

        _ ->
            Nothing


cons =
    (::)


headOfSingleton : List a -> Maybe a
headOfSingleton list =
    case list of
        h :: [] ->
            Just h

        _ ->
            Nothing


indexOf =
    List.Extra.elemIndex


findIndex =
    List.Extra.findIndex


listLast =
    List.reverse >> List.head


listLastOr default =
    listLast >> Maybe.withDefault default


allEq v ls =
    List.all (eq v) ls


groupEqBy : (a -> b) -> List a -> List ( a, List a )
groupEqBy =
    List.Extra.gatherEqualsBy


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst =
    List.Extra.find


listPadRight : a -> Int -> List a -> List a
listPadRight default toLength list =
    let
        fromLength =
            List.length list
    in
    case compare fromLength toLength of
        LT ->
            list ++ List.repeat (toLength - fromLength) default

        EQ ->
            list

        GT ->
            List.take toLength list



-- RESULT HELPERS


resultConcat : List (Result a b) -> Result (List a) (List b)
resultConcat =
    Result.Extra.partition
        >> (\( os, es ) ->
                case es of
                    [] ->
                        Ok os

                    _ ->
                        Err es
           )



-- MAYBE HELPERS


orElseLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orElseLazy fn mb =
    case mb of
        Nothing ->
            fn ()

        _ ->
            mb


replaceNothingWith : a -> Maybe a -> Maybe a
replaceNothingWith a maybe =
    case maybe of
        Nothing ->
            Just a

        _ ->
            maybe


maybeFilter : (a -> Bool) -> Maybe a -> Maybe a
maybeFilter pred =
    Maybe.andThen
        (\v ->
            if pred v then
                Just v

            else
                Nothing
        )


maybeFromPred : (a -> Bool) -> a -> Maybe a
maybeFromPred pred v =
    if pred v then
        Just v

    else
        Nothing


maybeFromBool : Bool -> a -> Maybe a
maybeFromBool bool v =
    if bool then
        Just v

    else
        Nothing


maybeToBool : Maybe a -> Bool
maybeToBool maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


maybeOneOf : List (Maybe a) -> Maybe a
maybeOneOf maybes =
    case maybes of
        [] ->
            Nothing

        Nothing :: rest ->
            maybeOneOf rest

        (Just a) :: _ ->
            Just a



-- Browser Helpers


bElement :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
bElement =
    Browser.element


bDocument :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
bDocument =
    Browser.document


browserApplication :
    { init : flags -> Url -> Key -> ( model, Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url -> msg
    }
    -> Program flags model msg
browserApplication =
    Browser.application


animationApp : (Float -> Html Float) -> Program () Float Float
animationApp vfn =
    Browser.element
        { init = \() -> ( 0, Cmd.none )
        , subscriptions = \_ -> Browser.Events.onAnimationFrameDelta (clamp 0 100)
        , update = \delta elapsed -> ( elapsed + delta, Cmd.none )
        , view = vfn
        }


notifyClick : msg -> Attribute msg
notifyClick =
    Html.Events.onClick


notifyPointerDown : msg -> Attribute msg
notifyPointerDown msg =
    Html.Events.on "pointerdown" (JD.succeed msg)


notifyPointerEnter : msg -> Attribute msg
notifyPointerEnter msg =
    Html.Events.on "pointerenter" (JD.succeed msg)


notifyPointerUp : msg -> Attribute msg
notifyPointerUp msg =
    Html.Events.on "pointerup" (JD.succeed msg)


notifyPointerCancel : msg -> Attribute msg
notifyPointerCancel msg =
    Html.Events.on "pointercancel" (JD.succeed msg)


disableContextMenu msg =
    Html.Events.custom "contextmenu"
        (JD.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )


onInput : (String -> msg) -> Html.Attribute msg
onInput =
    Html.Events.onInput



-- RANDOM UTILS


type alias Generator a =
    Random.Generator a


type alias Seed =
    Random.Seed


rAndMap =
    Random.Extra.andMap


rCombine =
    Random.Extra.combine


stepWithInitialSeed : Int -> Random.Generator a -> a
stepWithInitialSeed i gen =
    Random.step gen (Random.initialSeed i) |> first


randomDigitChar : Generator Char
randomDigitChar =
    Random.Char.char (Char.toCode '0') (Char.toCode '9')


randomId : Generator String
randomId =
    randomAlphaNumericStringOfLength 20


randomAlphaNumericStringOfLength : Int -> Generator String
randomAlphaNumericStringOfLength length =
    Random.weighted ( 10, randomDigitChar )
        [ ( 26, Random.Char.lowerCaseLatin )
        , ( 26, Random.Char.upperCaseLatin )
        ]
        |> Random.andThen identity
        |> Random.list length
        |> Random.map String.fromList


randomFloatT : Float2 -> Generator Float
randomFloatT ( a, b ) =
    Random.float a b



-- STYLE HELPERS


maybeAttr : (a -> Attribute msg) -> Maybe a -> Attribute msg
maybeAttr fn mb =
    case mb of
        Just x ->
            fn x

        Nothing ->
            noAttr


noAttr =
    style "" ""


resizeNone =
    style "resize" "none"


classNames names =
    HA.attribute "class" (String.join " " names)


bottom100 =
    style "bottom" "100%"


top100 =
    style "top" "100%"


left100 =
    style "left" "100%"


right100 =
    style "right" "100%"


left0 =
    style "left" "0"


top0 =
    style "top" "0"


bottom0 =
    style "bottom" "0"


right0 =
    style "right" "0"


opacity =
    fromFloat >> style "opacity"


opacityFromBool bool =
    if bool then
        opacity 1

    else
        opacity 0



-- HTML UTILS


keyedDiv =
    Html.Keyed.node "div"


noView : Html msg
noView =
    text ""


viewBool : Bool -> Html msg -> Html msg
viewBool bool x =
    if bool then
        x

    else
        noView


maybeView : (a -> Html msg) -> Maybe a -> Html msg
maybeView fn mb =
    case mb of
        Just x ->
            fn x

        Nothing ->
            noView



-- COLOR UTILS
--  rainbow : [ 'dark', 'light', 'blue', 'purple', 'red', 'orange', 'yellow', 'green' ],
-- colors = [black, white, orange , red, green, purple, blue]


rainbow =
    [ "black", "white", "blue", "purple", "red", "orange", "yellow", "green" ]


wPurple =
    "#BF00FF"


wGreen_lime =
    "#BFFF00"


wYellow =
    "#FAFA00"


wOrange =
    "#FFBF00"


wPink =
    "#FF00BF"


wBlue =
    "#00BFFF"


wGreen2_sea =
    "#00FFBF"


wBlack =
    "#202020"


wGray =
    "#2b2b2b"


wLightGray =
    "#373737"


wWhite =
    "#fcfcfc"


windowsAccentColorRainbow__ =
    [ wPurple
    , wGreen_lime
    , wYellow
    , wOrange
    , wPink
    , wBlue
    , wGreen2_sea
    , wBlack
    , wGray
    , wLightGray
    , wWhite
    ]


randomColor__ : Generator String
randomColor__ =
    randomHue__ |> Random.map (\h -> hsl h 1 0.5)


randomHue__ : Generator Float
randomHue__ =
    let
        sampleCount =
            10
    in
    normSamples (sampleCount + 1)
        |> List.drop 1
        |> List.take (sampleCount - 1)
        |> Random.uniform 0



-- SCREEN HELPERS


type alias Screen =
    { width : Float
    , height : Float
    , top : Float
    , left : Float
    , right : Float
    , bottom : Float
    }


initialScreen : Screen
initialScreen =
    toScreen 600 600


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = -height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = height / 2
    }


getScreenTask : Task.Task x Screen
getScreenTask =
    Browser.Dom.getViewport
        |> Task.map (.viewport >> (\{ width, height } -> toScreen width height))


getScreenCmd : (Screen -> msg) -> Cmd msg
getScreenCmd msg =
    Task.perform msg getScreenTask


onScreenResized : (Screen -> msg) -> Sub msg
onScreenResized msg =
    Browser.Events.onResize (\w h -> toScreen (toFloat w) (toFloat h))
        |> Sub.map msg



-- NEL NON EMPTY LIST


type alias NEL a =
    ( a, List a )



-- SET


insertMaybe : Maybe comparable -> Set comparable -> Set comparable
insertMaybe mb =
    case mb of
        Just v ->
            Set.insert v

        Nothing ->
            identity


setToggleMember e s =
    if Set.member e s then
        Set.remove e s

    else
        Set.insert e s



-- JSON DECODE HELPERS


jdAndMap : Decoder a -> Decoder (a -> b) -> Decoder b
jdAndMap =
    JD.map2 (|>)


jdWhen : Decoder Bool -> Decoder a -> Decoder a -> Decoder a
jdWhen pred true false =
    JD.andThen
        (\exists ->
            if exists then
                true

            else
                false
        )
        pred



-- JSON DECODE PIPELINE HELPERS


jpRequired : String -> Decoder a -> Decoder (a -> b) -> Decoder b
jpRequired field decoder =
    JD.map2 (|>) (JD.field field decoder)


jpOptional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
jpOptional field decoder fallback =
    let
        fieldExists =
            JD.maybe (JD.field field JD.value) |> JD.map maybeToBool

        fieldDecoder =
            JD.field field (JD.oneOf [ JD.null fallback, decoder ])
    in
    JD.map2 (|>)
        (jdWhen fieldExists fieldDecoder (JD.succeed fallback))


jpHardcoded : a -> Decoder (a -> b) -> Decoder b
jpHardcoded a =
    JD.map2 (|>) (JD.succeed a)



-- SET UTILS


setFilterMap : (a -> Maybe comparable) -> Set a -> Set comparable
setFilterMap fn =
    Set.foldl
        (\v acc ->
            case fn v of
                Nothing ->
                    acc

                Just nv ->
                    Set.insert nv acc
        )
        Set.empty



-- LCR UTILS


type alias LCR a =
    ( List a, a, List a )


lcrToList : LCR a -> List a
lcrToList ( l, c, r ) =
    l ++ c :: r


lcrMap : (a -> b) -> LCR a -> LCR b
lcrMap fn ( l, c, r ) =
    ( List.map fn l, fn c, List.map fn r )


lcrMapCS : (a -> b) -> (a -> b) -> LCR a -> LCR b
lcrMapCS fc fs ( l, c, r ) =
    ( List.map fs l, fc c, List.map fs r )


lcrRange : Int -> Int -> Int -> LCR Int
lcrRange lo c hi =
    ( List.range lo (c - 1)
    , c
    , List.range (c + 1) hi
    )


lcrFromPivot : Pivot a -> LCR a
lcrFromPivot p =
    ( Pivot.getL p, Pivot.getC p, Pivot.getR p )



-- DEBUG


tapBy fn e =
    let
        _ =
            Debug.log "tap: " (fn e)
    in
    e
