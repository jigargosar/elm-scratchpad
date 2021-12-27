module Anime.AnimeClone_V2 exposing (main)

import Anime.Anim as A
import Ease
import Pivot exposing (Pivot)
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { clock : A.Clock
    , examples : Pivot Example
    }


type Example
    = Example_Staggering_Basics
    | Example_Staggering_StartValue
    | Example_Staggering_Range
    | Example_Staggering_FromCenter


init : () -> ( Model, Cmd Msg )
init () =
    ( { clock = A.initClock
      , examples =
            Pivot.fromCons
                Example_Staggering_Basics
                [ Example_Staggering_StartValue
                , Example_Staggering_Range
                , Example_Staggering_FromCenter
                ]
      }
    , Cmd.none
    )


type Msg
    = NOP
    | OnClockMsg A.ClockMsg
    | ExampleClicked Example


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ A.clockSubscription OnClockMsg
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnClockMsg clockMsg ->
            ( { model | clock = A.updateClock clockMsg model.clock }
            , Cmd.none
            )

        ExampleClicked example ->
            ( case Pivot.firstWith (eq example) model.examples of
                Nothing ->
                    model

                Just examples ->
                    { model | examples = examples, clock = A.initClock }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    Document "Anime V2"
        [ basicStylesNode
        , div [ fg green ]
            (model.examples
                |> Pivot.mapCS
                    (viewExample True model.clock)
                    (viewExample False A.initClock)
                |> Pivot.toList
            )
        ]


viewExample : Bool -> A.Clock -> Example -> Html Msg
viewExample isSelected animClock eg =
    let
        info =
            exampleInfo eg
    in
    div
        [ style "border-bottom" "1px solid rgba(0,0,0,0.65)"
        , pb "20px"
        , sMaxWidth "600px"
        , positionRelative
        , notifyClick (ExampleClicked eg)
        , cursorPointer
        ]
        [ viewExampleBackground isSelected
        , viewExampleTitle isSelected info.title
        , fRow [ contentCenter ]
            [ div [ styleWidth "350px" ]
                [ info.view isSelected animClock
                ]
            ]
        ]


viewExampleBackground : Bool -> Html msg
viewExampleBackground isSelected =
    div
        [ positionAbsolute
        , w100
        , h100
        , bgCurrentColor
        , opacity
            (if isSelected then
                0.05

             else
                0
            )
        ]
        []


viewExampleTitle : Bool -> String -> Html msg
viewExampleTitle isSelected t =
    div
        [ if isSelected then
            fgCurrentColor

          else
            fg (whiteA 0.9)
        , transitionFG
        , pa "20px"
        , ttu
        , fontSize "20px"
        ]
        [ text t ]


exampleInfo : Example -> { title : String, view : Bool -> A.Clock -> Html msg }
exampleInfo example =
    case example of
        Example_Staggering_Basics ->
            { title = "Staggering basics"
            , view = viewStaggeringBasicsExample
            }

        Example_Staggering_StartValue ->
            { title = "start value"
            , view = viewStaggeringStartValueExample
            }

        Example_Staggering_Range ->
            { title = "Range Value"
            , view = viewStaggerRangeValueExample
            }

        Example_Staggering_FromCenter ->
            { title = "From Value"
            , view = viewStaggerFromCenterExample
            }


viewStaggeringBasicsExample : Bool -> A.Clock -> Html msg
viewStaggeringBasicsExample isSelected ac =
    timesWithIndexAndLength 6
        (\il ->
            let
                dx =
                    A.value
                        [ A.to 270
                        , A.duration 1800
                        , A.ease Ease.outElastic
                        , A.delay <| round <| A.stagger 100 il
                        ]
                        ac

                labelText =
                    [ "delay = (100 * ", fromInt il.index, ") ms" ]
                        |> String.join ""
            in
            div [ positionRelative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewLabel isSelected labelText
                , viewSquare
                    [ smallSizeStyles
                    , [ style "transform" ("translateX(" ++ fromFloat dx ++ "px)")
                      ]
                    ]
                ]
        )
        |> fCol [ gap "10px" ]


viewStaggeringStartValueExample : Bool -> A.Clock -> Html msg
viewStaggeringStartValueExample isSelected ac =
    timesWithIndexAndLength 6
        (\il ->
            let
                dx =
                    A.value
                        [ A.to 270
                        , A.duration 1800
                        , A.ease Ease.outElastic
                        , A.delay <| round <| (500 + A.stagger 100 il)
                        ]
                        ac

                labelText =
                    [ "delay = 500 + (100 * ", fromInt il.index, ") ms" ]
                        |> String.join ""
            in
            div [ positionRelative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewLabel isSelected labelText
                , viewSquare
                    [ smallSizeStyles
                    , [ style "transform" ("translateX(" ++ fromFloat dx ++ "px)")
                      ]
                    ]
                ]
        )
        |> fCol [ gap "10px" ]


viewStaggerFromCenterExample : Bool -> A.Clock -> Html msg
viewStaggerFromCenterExample _ ac =
    timesWithIndexAndLength 6
        (\il ->
            let
                dx =
                    A.value
                        [ A.to 270
                        , A.duration 1800
                        , A.ease Ease.outElastic
                        , A.delay <| round <| A.staggerFromCenter 200 il
                        ]
                        ac
            in
            div [ positionRelative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewSquare
                    [ smallSizeStyles
                    , [ style "transform" ("translateX(" ++ fromFloat dx ++ "px)")
                      ]
                    ]
                ]
        )
        |> fCol [ gap "10px" ]


viewStaggerRangeValueExample : Bool -> A.Clock -> Html msg
viewStaggerRangeValueExample isSelected ac =
    fCol [ gap "10px" ]
        (let
            length =
                6

            labelText index =
                [ "rotate = -360 + ((360 - (-360)) / "
                , fromInt (length - 1)
                , ") * "
                , fromInt index
                ]
                    |> String.join ""
         in
         rangeN length
            |> mapWithIndexAndLength
                (\il index ->
                    let
                        commonAttrs =
                            [ A.duration 1200
                            , A.ease Ease.inOutQuad
                            ]

                        dx =
                            A.value (A.to 270 :: commonAttrs) ac

                        da =
                            A.value
                                (A.to (A.staggerRange ( -360, 360 ) il)
                                    --:: A.toStaggered (A.staggerRange ( -360, 360 ))
                                    :: commonAttrs
                                )
                                ac
                    in
                    div [ positionRelative ]
                        [ viewSquare [ smallSizeStyles, shadowElStyles ]
                        , viewLabel isSelected (labelText index)
                        , viewSquare
                            [ smallSizeStyles
                            , [ transforms
                                    [ "translateX(" ++ fromFloat dx ++ "px)"
                                    , "rotate(" ++ fromFloat da ++ "deg)"
                                    ]
                              ]
                            ]
                        ]
                )
        )


green =
    hsl 0.42 1 0.5


shadowElStyles =
    [ positionAbsolute, left0, opacity 0.2 ]


smallSizeStyles =
    [ styleWidth "20px", styleHeight "20px" ]


viewSquare attrs =
    div (bgCurrentColor :: List.concat attrs) []


viewLabel : Bool -> String -> Html msg
viewLabel isSelected t =
    div
        ([ positionAbsolute
         , opacity
            (if isSelected then
                0.3

             else
                0
            )
         , transitionOpacity
         , fontSize "16px"
         , style "white-space" "nowrap"
         ]
            ++ [ pl "32px" ]
        )
        [ text t ]
