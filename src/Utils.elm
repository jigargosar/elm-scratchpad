module Utils exposing (..)

import Color
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Json.Decode as JD exposing (Decoder)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT


type Dir4
    = Up
    | Down
    | Left
    | Right


allDir4 : List Dir4
allDir4 =
    [ Up, Down, Left, Right ]


keyDecoder : Decoder String
keyDecoder =
    JD.field "key" JD.string


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


type alias Num2 number =
    ( number, number )


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


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


rangeN : Int -> List Int
rangeN n =
    List.range 0 (n - 1)


type alias Vec =
    { x : Float, y : Float }


vFromGP : GPos -> Vec
vFromGP ( gx, gy ) =
    vec (toFloat gx) (toFloat gy)


vec : Float -> Float -> Vec
vec =
    Vec


vScale : Float -> Vec -> Vec
vScale s { x, y } =
    vec (s * x) (s * y)


vSub1 : Float -> Vec -> Vec
vSub1 a { x, y } =
    vec (x - a) (y - a)


vAdd1 : Float -> Vec -> Vec
vAdd1 a { x, y } =
    vec (x + a) (y + a)


group =
    Svg.g


keyedGroup : List (Attribute msg) -> List ( String, Svg msg ) -> Svg msg
keyedGroup =
    Svg.Keyed.node "g"


xf =
    TA.transform


mv2 =
    TT.Translate


mvUp y =
    mv2 0 -y


mvRight x =
    mv2 x 0


scale s =
    TT.Scale s s


mv { x, y } =
    mv2 x y


black =
    grayN 0.1


gray =
    grayN 0.2


white =
    grayN 1


grayN n =
    rgb n n n


rgb r g b =
    Color.rgb r g b |> Color.toCssString


bgc =
    style "background-color"


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


saWidth =
    SA.width << String.fromFloat


saHeight =
    SA.height << String.fromFloat


strokeW =
    SA.strokeWidth << String.fromFloat


fill =
    SA.fill


stroke =
    SA.stroke


stylesNode : String -> Html msg
stylesNode string =
    Html.node "style" [] [ Html.text string ]


overflowVisible =
    style "overflow" "visible"


overflowHidden =
    style "overflow" "hidden"


fontSize =
    style "font-size"


transitionTransform =
    style "transition" "transform 300ms"


ffMonospace =
    style "font-family" "monospace"


style =
    Html.Attributes.style


tac =
    style "text-align" "center"


gap =
    style "gap"


contentCenter =
    style "justify-content" "center"


itemsCenter =
    style "align-items" "center"


pAll =
    style "padding"


dFlex =
    style "display" "flex"


dBlock =
    style "display" "block"


dGrid =
    style "display" "grid"


fDCol =
    style "flex-direction" "column"


noUserSelect =
    style "user-select" "none"


circle : Float -> List (Attribute msg) -> Svg msg
circle r xs =
    Svg.circle (Px.r r :: xs) []


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
rect w h xs =
    Svg.rect
        (saWidth w
            :: saHeight h
            :: Px.x (w / -2)
            :: Px.y (h / -2)
            :: xs
        )
        []


wordsAlignXLeft =
    TA.textAnchor TT.AnchorStart


wordsAlignXRight =
    TA.textAnchor TT.AnchorEnd


wordsAlignYTop =
    TA.dominantBaseline TT.DominantBaselineHanging


square : Float -> List (Attribute msg) -> Svg msg
square sz =
    rect sz sz


words : String -> List (Svg.Attribute msg) -> Svg msg
words str xs =
    Svg.text_
        (SA.textAnchor "middle"
            :: SA.dominantBaseline "central"
            :: xs
        )
        [ Svg.text str ]



-- BASICS


fdiv =
    (/)


mul =
    (*)


eq =
    (==)


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


first =
    Tuple.first


second =
    Tuple.second


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


headOfSingleton : List a -> Maybe a
headOfSingleton list =
    case list of
        h :: [] ->
            Just h

        _ ->
            Nothing


{-| Only when `from` is member and `to` is not member.
-}
moveValueFromKeyToKey : comparable -> comparable -> Dict comparable v -> Dict comparable v
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


filterKey : (comparable -> Bool) -> Dict comparable b -> Dict comparable b
filterKey fn =
    Dict.filter (\k _ -> fn k)


rejectKey : (comparable -> Bool) -> Dict comparable b -> Dict comparable b
rejectKey fn =
    filterKey (fn >> not)


filterValue : (b -> Bool) -> Dict comparable b -> Dict comparable b
filterValue fn =
    Dict.filter (\_ v -> fn v)


partitionKey : (comparable -> Bool) -> Dict comparable b -> ( Dict comparable b, Dict comparable b )
partitionKey fn d =
    ( filterKey fn d, rejectKey fn d )


renameKey : (a -> comparable) -> Dict a v -> Dict comparable v
renameKey fn =
    Dict.toList >> List.map (Tuple.mapFirst fn) >> Dict.fromList


mapSecond =
    Tuple.mapSecond


dictGet2 : comparable -> comparable -> Dict comparable v -> Maybe ( v, v )
dictGet2 a b dict =
    Maybe.map2 Tuple.pair (Dict.get a dict) (Dict.get b dict)


mapFirst =
    Tuple.mapFirst


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


sumBy : (a -> number) -> List a -> number
sumBy fn =
    List.foldl (fn >> add) 0


maxBy : (a -> number) -> List a -> number
maxBy fn =
    List.foldl (fn >> max) 0


atLeast =
    max


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


minBy : (a -> comparable) -> a -> a -> a
minBy fn a b =
    if fn a < fn b then
        a

    else
        b


insertBy : (v -> comparable) -> v -> Dict comparable v -> Dict comparable v
insertBy keyFn v =
    Dict.insert (keyFn v) v
