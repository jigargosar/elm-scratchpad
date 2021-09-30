module Main exposing (main)

import Html.Attributes exposing (style)
import Random exposing (Generator)
import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT


wc =
    { w = 710, h = 400 }


type alias Model =
    { current : ( Float, Float )
    , history : List ( Point, Point )
    , seed : Random.Seed
    }


initialModel : Model
initialModel =
    { current = ( wc.w / 2, wc.h / 2 )
    , history = []
    , seed = Random.initialSeed 0
    }


makeMove : Model -> Model
makeMove model =
    let
        motionGenerator =
            Random.float -6 6

        ( ( dx, dy ), seed ) =
            Random.step
                (Random.pair motionGenerator motionGenerator)
                model.seed

        ( x, y ) =
            model.current

        next =
            ( clamp 0 wc.w (x + dx)
            , clamp 0 wc.h (y + dy)
            )
    in
    { model
        | current = next
        , seed = seed
        , history = ( next, model.current ) :: model.history
    }


makeNMoves : Int -> Model -> Model
makeNMoves n model =
    if n <= 0 then
        model

    else
        makeNMoves (n - 1) (makeMove model)


main =
    let
        model =
            initialModel
                |> makeNMoves 2000
    in
    Svg.svg
        [ style "font-size" "20px"
        , style "background-color" "#333"
        , SA.width <| String.fromFloat wc.w
        , SA.height <| String.fromFloat wc.h
        ]
        [ viewMotion model
        , Svg.g [ TA.transform [ TT.Translate 50 50 ] ]
            [ drawWalk 0 "blue"
            , drawWalk 1 "green"
            , drawWalk 2 "red"
            , drawWalk 3 "purple"
            , Svg.circle [ Px.r 5 ] []
            ]
        ]


viewMotion model =
    List.indexedMap (\i ( a, b ) -> drawLine_V2 a b 0.5) model.history
        |> Svg.g [ SA.stroke "white" ]


drawWalk i c =
    Random.step randomWalk_V2 (Random.initialSeed i)
        |> (\( pts, _ ) -> drawLines [ SA.stroke c ] pts)


type alias Point =
    ( Float, Float )


randomWalk_V2 : Generator (List Point)
randomWalk_V2 =
    let
        r =
            50

        anglesToWalk ls =
            List.foldl
                (\a ( pt, pts ) ->
                    ( shiftByRTheta r a pt, pt :: pts )
                )
                ( ( 0, 0 ), [] )
                ls
                |> Tuple.second
    in
    Random.list 100 randomAngle
        |> Random.map anglesToWalk


shiftByRTheta r theta ( x, y ) =
    let
        ( dx, dy ) =
            fromPolar ( r, theta )
    in
    ( x + dx, y + dy )


randomAngle =
    Random.float 0 (turns 1)


drawLines attrs pts =
    let
        len =
            toFloat (List.length pts)

        foo i ( a, b ) =
            drawLine_V2 a b ((toFloat i + 1) / len)

        lines =
            List.map2 Tuple.pair pts (List.drop 1 pts)
                |> List.indexedMap foo
    in
    Svg.g (SA.strokeWidth "3" :: SA.stroke "black" :: attrs) lines


drawLine_V2 a b o =
    Svg.polyline
        [ TA.points [ a, b ]
        , TA.opacity (TT.Opacity o)
        ]
        []
