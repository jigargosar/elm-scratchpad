module Utils exposing (..)

import Browser
import Color
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT


type alias GPos =
    ( Int, Int )


map2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
map2 fn ( a, b ) ( c, d ) =
    ( fn a c, fn b d )


adjacentGPSInGridOf : Int -> Int -> GPos -> List GPos
adjacentGPSInGridOf w h gp =
    let
        adjacentOffsets =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]

        gpAdd : GPos -> GPos -> GPos
        gpAdd ( a, b ) ( c, d ) =
            ( a + c, b + d )

        validateGP m =
            rangeWH w h |> List.member m
    in
    adjacentOffsets
        |> List.map (gpAdd gp)
        |> List.filter validateGP


areAdjacent : GPos -> GPos -> Bool
areAdjacent gp1 gp2 =
    let
        adjacentOffsets =
            [ ( 1, 0 ), ( 0, 1 ), ( -1, 0 ), ( 0, -1 ) ]

        gpAdd : GPos -> GPos -> GPos
        gpAdd ( a, b ) ( c, d ) =
            ( a + c, b + d )
    in
    adjacentOffsets
        |> List.map (gpAdd gp1)
        |> List.member gp2


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


fillTransparent =
    fill "transparent"


noStroke =
    stroke "none"


saWidth =
    SA.width << String.fromFloat


saHeight =
    SA.height << String.fromFloat


fill =
    SA.fill


stroke =
    SA.stroke


overflowVisible =
    style "overflow" "visible"


fontSize =
    style "font-size"


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


headOfSingleton : List a -> Maybe a
headOfSingleton list =
    case list of
        h :: [] ->
            Just h

        _ ->
            Nothing


{-| Only when `from` is member and `to` is not member.
-}
dictMoveValueFromTo : comparable -> comparable -> Dict comparable v -> Dict comparable v
dictMoveValueFromTo from to dict =
    case ( Dict.get from dict, Dict.get to dict ) of
        ( Just value, Nothing ) ->
            dict
                |> Dict.remove from
                |> Dict.insert to value

        _ ->
            dict


first =
    Tuple.first


second =
    Tuple.second


swapKeyValueBy : (v -> comparable) -> Dict k v -> Dict comparable k
swapKeyValueBy fn =
    Dict.foldl (\k v -> Dict.insert (fn v) k) Dict.empty


filterKey : (comparable -> Bool) -> Dict comparable b -> Dict comparable b
filterKey fn =
    Dict.filter (\k _ -> fn k)


filter : (a -> Bool) -> List a -> List a
filter =
    List.filter


reject : (a -> Bool) -> List a -> List a
reject fn =
    filter (fn >> not)


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


mapFirst =
    Tuple.mapFirst
