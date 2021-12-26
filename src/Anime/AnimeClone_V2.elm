module Anime.AnimeClone_V2 exposing (main)

import Anime.Anim as A
import Browser.Events
import Ease
import Json.Decode as JD
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
    { animClock : A.AnimClock
    , examples : Pivot Example
    }


type Example
    = ExampleStaggeringFromCenter
    | ExampleStaggeringRange


init : () -> ( Model, Cmd Msg )
init () =
    ( { animClock = A.animClockInit
      , examples =
            Pivot.fromCons
                ExampleStaggeringRange
                [ ExampleStaggeringFromCenter
                ]
      }
    , Cmd.none
    )


type Msg
    = NOP
    | OnAnimClockDelta A.AnimClockDelta
    | OnClick
    | ExampleClicked Example


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ A.animClockSubscription OnAnimClockDelta
    , Browser.Events.onClick (JD.succeed OnClick)
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnAnimClockDelta delta ->
            ( { model
                | animClock = A.animClockUpdateOnDelta delta model.animClock
              }
            , Cmd.none
            )

        OnClick ->
            ( { model | animClock = A.animClockInit }, Cmd.none )

        ExampleClicked example ->
            ( { model | examples = withRollback (Pivot.firstWith (eq example)) model.examples }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    Document "Anime V2"
        [ basicStylesNode
        , div [ fg green ]
            (model.examples
                |> Pivot.mapCS
                    (viewExample True model.animClock)
                    (viewExample False A.animClockInit)
                |> Pivot.toList
            )
        ]


viewExample : Bool -> A.AnimClock -> Example -> Html Msg
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


exampleInfo : Example -> { title : String, view : Bool -> A.AnimClock -> Html msg }
exampleInfo example =
    case example of
        ExampleStaggeringRange ->
            { title = "Range Value"
            , view = viewStaggerRangeValueExample
            }

        ExampleStaggeringFromCenter ->
            { title = "From Value"
            , view = viewStaggerFromCenterExample
            }


viewStaggerFromCenterExample : Bool -> A.AnimClock -> Html msg
viewStaggerFromCenterExample _ ac =
    timesWithIndexAndLength 6
        (\il ->
            let
                dx =
                    A.valueOf
                        [ A.fromTo 0 270
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


viewStaggerRangeValueExample : Bool -> A.AnimClock -> Html msg
viewStaggerRangeValueExample isSelected ac =
    timesWithIndexAndLength 6
        (\il ->
            let
                commonAttrs =
                    [ A.duration 1200
                    , A.ease Ease.inOutQuad
                    ]

                dx =
                    A.valueOf (A.fromTo 0 270 :: commonAttrs) ac

                da =
                    A.valueOf
                        (A.fromTo 0 (A.staggerRange ( -360, 360 ) il)
                            :: commonAttrs
                        )
                        ac

                labelText =
                    [ "rotate = -360 + ((360 - (-360)) / "
                    , fromInt (il.length - 1)
                    , ") * "
                    , fromInt il.index
                    ]
                        |> String.join ""
            in
            div [ positionRelative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewLabel isSelected labelText [ pl "32px" ]
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
        |> fCol [ gap "10px" ]


green =
    hsl 0.42 1 0.5


shadowElStyles =
    [ positionAbsolute, left0, opacity 0.2 ]


smallSizeStyles =
    [ styleWidth "20px", styleHeight "20px" ]


viewSquare attrs =
    div (bgCurrentColor :: List.concat attrs) []


viewLabel : Bool -> String -> List (Attribute msg) -> Html msg
viewLabel isSelected t aa =
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
            ++ aa
        )
        [ text t ]
