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
    | Example_Staggering_Direction
    | Example_Staggering_Easing


init : () -> ( Model, Cmd Msg )
init () =
    ( { clock = A.initClock
      , examples =
            Pivot.fromCons
                Example_Staggering_Basics
                [ Example_Staggering_StartValue
                , Example_Staggering_Range
                , Example_Staggering_FromCenter
                , Example_Staggering_Direction
                , Example_Staggering_Easing
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
        , relative
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
        [ absolute
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

        Example_Staggering_Direction ->
            { title = "Direction"
            , view = viewStaggeringReverseDirectionExample
            }

        Example_Staggering_Easing ->
            { title = "Easing"
            , view = viewStaggeringEasingExample
            }


viewStaggeringBasicsExample : Bool -> A.Clock -> Html msg
viewStaggeringBasicsExample isSelected ac =
    let
        labelText index =
            [ "delay = (100 * ", fromInt index, ") ms" ]
                |> String.join ""

        viewWithDX dx index =
            div [ relative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewLabel isSelected (labelText index)
                , viewSquare [ smallSizeStyles, [ transforms [ translateX dx ] ] ]
                ]
    in
    fCol [ gap "10px" ]
        (rangeN 6
            |> A.mapListForOneValue
                [ A.to 270
                , A.duration 1800
                , A.ease Ease.outElastic
                , A.staggerDelay (A.stagger 100)
                ]
                viewWithDX
                ac
        )


viewStaggeringStartValueExample : Bool -> A.Clock -> Html msg
viewStaggeringStartValueExample isSelected ac =
    let
        labelText index =
            [ "delay = 500 + (100 * ", fromInt index, ") ms" ]
                |> String.join ""

        viewWithDX dx index =
            div [ relative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewLabel isSelected (labelText index)
                , viewSquare [ smallSizeStyles, [ transforms [ translateX dx ] ] ]
                ]
    in
    fCol [ gap "10px" ]
        (rangeN 6
            |> A.mapListForOneValue
                [ A.to 270
                , A.duration 1800
                , A.ease Ease.outElastic
                , A.staggerDelay (A.stagger 100 >> add 500)
                ]
                viewWithDX
                ac
        )


viewStaggerFromCenterExample : Bool -> A.Clock -> Html msg
viewStaggerFromCenterExample _ ac =
    let
        viewWithDX dx _ =
            div [ relative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewSquare [ smallSizeStyles, [ transforms [ translateX dx ] ] ]
                ]
    in
    fCol [ gap "10px" ]
        (rangeN 6
            |> A.mapListForOneValue
                [ A.to 270
                , A.duration 1800
                , A.ease Ease.outElastic
                , A.staggerDelay (A.staggerFromCenter 200)
                ]
                viewWithDX
                ac
        )


viewStaggerRangeValueExample : Bool -> A.Clock -> Html msg
viewStaggerRangeValueExample isSelected clock =
    let
        length =
            6

        labelText index =
            [ "rotate = -360 + ((360 - (-360)) / "
            , fromInt (length - 1)
            , ") * "
            , fromInt index
            ]
                |> String.join ""

        viewWithDXAndDA dx da index =
            div [ relative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewLabel isSelected (labelText index)
                , viewSquare
                    [ smallSizeStyles
                    , [ transforms [ translateX dx, rotateDeg da ]
                      ]
                    ]
                ]

        common =
            [ A.duration 1200, A.ease Ease.inOutQuad ]
    in
    fCol [ gap "10px" ]
        (A.mapListFor2Values
            (A.to 270 :: common)
            (A.staggerTo (A.staggerRange ( -360, 360 )) :: common)
            viewWithDXAndDA
            clock
            (rangeN length)
        )


viewStaggeringReverseDirectionExample : Bool -> A.Clock -> Html msg
viewStaggeringReverseDirectionExample isSelected ac =
    let
        length =
            6

        labelText index =
            [ "delay = (100 * ", fromInt (length - 1 - index), ") ms" ]
                |> String.join ""

        viewWithDX dx index =
            div [ relative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewLabel isSelected (labelText index)
                , viewSquare [ smallSizeStyles, [ transforms [ translateX dx ] ] ]
                ]
    in
    fCol [ gap "10px" ]
        (rangeN length
            |> A.mapListForOneValue
                [ A.to 270
                , A.duration 1800
                , A.ease Ease.outElastic
                , A.staggerDelay (A.staggerReverse 150)
                ]
                viewWithDX
                ac
        )


viewStaggeringEasingExample : Bool -> A.Clock -> Html msg
viewStaggeringEasingExample _ ac =
    let
        length =
            6

        viewWithDX dx _ =
            div [ relative ]
                [ viewSquare [ smallSizeStyles, shadowElStyles ]
                , viewSquare [ smallSizeStyles, [ transforms [ translateX dx ] ] ]
                ]
    in
    fCol [ gap "10px" ]
        (rangeN length
            |> A.mapListForOneValue
                [ A.to 270
                , A.duration 1800
                , A.ease Ease.outElastic
                , A.staggerDelay (A.staggerEase 300 Ease.outQuad)
                ]
                viewWithDX
                ac
        )


shadowElStyles =
    [ absolute, left0, opacity 0.2 ]


smallSizeStyles =
    [ styleWidth "20px", styleHeight "20px" ]


viewSquare attrs =
    div (bgCurrentColor :: List.concat attrs) []


viewLabel : Bool -> String -> Html msg
viewLabel isSelected t =
    div
        ([ absolute
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


translateX : Float -> String
translateX dx =
    "translateX(" ++ fromFloat dx ++ "px)"


rotateDeg : Float -> String
rotateDeg da =
    "rotate(" ++ fromFloat da ++ "deg)"
