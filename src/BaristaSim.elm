module BaristaSim exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href, rel, style, title)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SA
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Strainer
    = StrainerEmpty
    | StrainerWithCoffeePowder
    | StrainerWithWaste


type HandHoldable
    = HH_Strainer Strainer
    | HH_CoffeeCup CoffeeCup



--| HH_MilkCarton
--| HH_MilkJar


type CoffeeCup
    = CoffeeCupEmpty
    | CoffeeCupWithEspresso


type Order
    = Espresso


type DesktopHolder
    = DH_CoffeeCup CoffeeCup


type CheckoutHolder
    = CH_CoffeeCup CoffeeCup


type alias EspressoMaker =
    ( Maybe Strainer, Maybe CoffeeCup )


type alias Model =
    { strainerHolderA : Maybe Strainer
    , coffeePowderDispenser : Maybe Strainer
    , coffeeMaker : EspressoMaker
    , hands : Maybe HandHoldable
    , desktopHolders : Dict Int DesktopHolder
    , checkoutHolders : Dict Int CheckoutHolder
    , orders : Dict Int Order
    }


init : Model
init =
    { strainerHolderA = Just StrainerEmpty
    , coffeePowderDispenser = Nothing
    , coffeeMaker = ( Nothing, Nothing )
    , hands = Nothing
    , desktopHolders = Dict.empty
    , checkoutHolders = Dict.empty
    , orders = Dict.fromList [ ( 0, Espresso ) ]
    }


type Msg
    = StrainerHolderAClicked
    | CoffeePowderDispenserClicked
    | CoffeeMakerStrainerHolderClicked
    | CoffeeMakerCupHolderClicked
    | TrashClicked
    | CoffeeCupStackClicked
    | DesktopHolderClicked Int
    | CheckoutHolderClicked Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        StrainerHolderAClicked ->
            case ( model.hands, model.strainerHolderA ) of
                ( Nothing, Just strainer ) ->
                    { model | hands = Just (HH_Strainer strainer), strainerHolderA = Nothing }

                ( Just (HH_Strainer strainer), Nothing ) ->
                    { model | hands = Nothing, strainerHolderA = Just strainer }

                ( _, _ ) ->
                    model

        CoffeePowderDispenserClicked ->
            case ( model.hands, model.coffeePowderDispenser ) of
                ( Nothing, Just strainer ) ->
                    { model | hands = Just (HH_Strainer strainer), coffeePowderDispenser = Nothing }

                ( Just (HH_Strainer StrainerEmpty), Nothing ) ->
                    { model | hands = Nothing, coffeePowderDispenser = Just StrainerWithCoffeePowder }

                ( _, _ ) ->
                    model

        TrashClicked ->
            case model.hands of
                Just (HH_Strainer _) ->
                    { model | hands = Just (HH_Strainer StrainerEmpty) }

                Just (HH_CoffeeCup _) ->
                    { model | hands = Nothing }

                _ ->
                    model

        DesktopHolderClicked i ->
            case ( model.hands, Dict.get i model.desktopHolders ) of
                ( Nothing, Just (DH_CoffeeCup cup) ) ->
                    { model
                        | hands = Just (HH_CoffeeCup cup)
                        , desktopHolders = Dict.remove i model.desktopHolders
                    }

                ( Just (HH_CoffeeCup cup), Nothing ) ->
                    { model
                        | hands = Nothing
                        , desktopHolders = Dict.insert i (DH_CoffeeCup cup) model.desktopHolders
                    }

                _ ->
                    model

        CheckoutHolderClicked i ->
            case ( model.hands, Dict.get i model.checkoutHolders ) of
                ( Nothing, Just (CH_CoffeeCup cup) ) ->
                    { model
                        | hands = Just (HH_CoffeeCup cup)
                        , checkoutHolders = Dict.remove i model.checkoutHolders
                    }

                ( Just (HH_CoffeeCup cup), Nothing ) ->
                    { model
                        | hands = Nothing
                        , checkoutHolders = Dict.insert i (CH_CoffeeCup cup) model.checkoutHolders
                    }
                        |> checkOutOrderNum i

                _ ->
                    model

        CoffeeMakerStrainerHolderClicked ->
            case ( model.hands, model.coffeeMaker ) of
                ( Just (HH_Strainer StrainerWithCoffeePowder), ( Nothing, cup ) ) ->
                    { model
                        | hands = Nothing
                        , coffeeMaker = ( Just StrainerWithCoffeePowder, cup ) |> tryToMakeCoffee
                    }

                ( Nothing, ( Just (StrainerWithWaste as strainer), cup ) ) ->
                    { model
                        | hands = Just (HH_Strainer strainer)
                        , coffeeMaker = ( Nothing, cup ) |> tryToMakeCoffee
                    }

                _ ->
                    model

        CoffeeMakerCupHolderClicked ->
            case ( model.hands, model.coffeeMaker ) of
                ( Just (HH_CoffeeCup cup), ( strainer, Nothing ) ) ->
                    { model
                        | hands = Nothing
                        , coffeeMaker =
                            ( strainer, Just cup )
                                |> tryToMakeCoffee
                    }

                ( Nothing, ( strainer, Just cup ) ) ->
                    { model | hands = Just (HH_CoffeeCup cup), coffeeMaker = ( strainer, Nothing ) }

                _ ->
                    model

        CoffeeCupStackClicked ->
            case model.hands of
                Nothing ->
                    { model | hands = Just (HH_CoffeeCup CoffeeCupEmpty) }

                _ ->
                    model


tryToMakeCoffee : EspressoMaker -> EspressoMaker
tryToMakeCoffee coffeeMaker =
    case coffeeMaker of
        ( Just StrainerWithCoffeePowder, Just CoffeeCupEmpty ) ->
            ( Just StrainerWithWaste, Just CoffeeCupWithEspresso )

        _ ->
            coffeeMaker


checkOutOrderNum : Int -> Model -> Model
checkOutOrderNum i model =
    case ( Dict.get i model.orders, Dict.get i model.checkoutHolders ) of
        ( Just Espresso, Just (CH_CoffeeCup CoffeeCupWithEspresso) ) ->
            { model | checkoutHolders = Dict.remove i model.checkoutHolders }

        _ ->
            model


view : Model -> Html Msg
view model =
    div []
        [ Html.node "link" [ rel "stylesheet", href "barista.css" ] []
        , div
            [ style "padding" "1.5rem"
            , class "flex-column gap1"
            , class "debug debug-c"
            ]
            [ gRow [ class "gap1" ]
                (List.range 0 2
                    |> List.map
                        (\i ->
                            col
                                [ class "gap05"
                                , onClick (CheckoutHolderClicked i)
                                ]
                                [ txt [] (Debug.toString (i + 1))
                                , txt [] (Debug.toString (Dict.get i model.orders))
                                , txt [] (Debug.toString (Dict.get i model.checkoutHolders))
                                ]
                        )
                )
            , gRow [ class "tac" ]
                [ fCol [ onClick StrainerHolderAClicked, class "" ]
                    [ txt [] "strainerHolderA"
                    , div [ title <| Debug.toString model.strainerHolderA ]
                        [ viewMaybeStrainer model.strainerHolderA ]
                    ]
                , fCol [ onClick StrainerHolderAClicked, class "" ]
                    [ txt [] "strainerHolderA"
                    , div [ title <| Debug.toString model.strainerHolderA ]
                        [ viewMaybeStrainer model.strainerHolderA ]
                    ]
                ]
            , fCol [ onClick CoffeeCupStackClicked ]
                [ txt [] "Coffee Cups Stack"
                ]
            , gRow
                [ class "gap1"
                ]
                [ viewEspressoMaker model.coffeeMaker
                , viewCoffeePowderDispenser model.coffeePowderDispenser
                ]
            , div [ onClick TrashClicked, class "flex-row gap1" ]
                [ txt [] "Trash"
                ]
            , div [ class "flex-row gap1" ]
                [ txt [] "Hands"
                , txt [] (Debug.toString model.hands)
                ]
            , div
                [ class "flex-column gap1" ]
                (List.range 0 4
                    |> List.map
                        (\i ->
                            div
                                [ class "flex-row gap1"
                                , onClick (DesktopHolderClicked i)
                                ]
                                [ txt [] (Debug.toString (i + 1))
                                , txt [] (Debug.toString (Dict.get i model.desktopHolders))
                                ]
                        )
                )
            ]
        ]


rect w h xs =
    Svg.rect (Px.x (-w / 2) :: Px.y (-h / 2) :: Px.width w :: Px.height h :: xs) []


viewBox2 w h =
    TA.viewBox (-w / 2) (-h / 2) w h


viewCoffeePowderDispenser : Maybe Strainer -> Html Msg
viewCoffeePowderDispenser mbStrainer =
    col
        [ class "debug-c"
        , title <| Debug.toString mbStrainer
        , onClick CoffeePowderDispenserClicked
        ]
        [ txt [] "Coffee Dispenser"
        , div [] [ viewMaybeStrainer mbStrainer ]
        , txt []
            (case mbStrainer of
                Just _ ->
                    "Strainer"

                Nothing ->
                    "|---|"
            )
        ]


viewMaybeStrainer mbStrainer =
    Svg.svg
        [ viewBox2 128 64
        , Px.width 128
        , SA.fill "none"
        , SA.stroke "none"

        --, SA.class "debug"
        ]
        (case mbStrainer of
            Nothing ->
                []

            Just strainer ->
                let
                    fillC =
                        case strainer of
                            StrainerEmpty ->
                                "#fff"

                            StrainerWithCoffeePowder ->
                                "brown"

                            StrainerWithWaste ->
                                "#000"
                in
                [ Svg.circle
                    [ SA.stroke "#000"
                    , Px.strokeWidth 4
                    , SA.fill "hsl(0.55turn 70% 50% / 1)"
                    , SA.fill fillC
                    , Px.r 16
                    , TA.transform [ TT.Translate -16 0 ]
                    ]
                    []
                , rect 32 16 [ SA.fill "#000", TA.transform [ TT.Translate 16 0 ] ]
                ]
        )


viewEspressoMaker : EspressoMaker -> Html Msg
viewEspressoMaker ( mbStrainer, mbCup ) =
    col [ class " debug-c" ]
        [ txt [] "Espresso Maker"
        , div
            [ onClick CoffeeMakerStrainerHolderClicked
            , title (Debug.toString mbStrainer)
            ]
            [ viewMaybeStrainer mbStrainer
            ]
        , txt
            [ onClick CoffeeMakerStrainerHolderClicked
            , title (Debug.toString mbStrainer)
            ]
            (case mbStrainer of
                Just _ ->
                    "Strainer"

                Nothing ->
                    "|---|"
            )
        , gRow [ onClick CoffeeMakerCupHolderClicked ]
            [ txt [ title (Debug.toString mbCup) ]
                (case mbCup of
                    Just _ ->
                        "Cup"

                    Nothing ->
                        "|---|"
                )
            , txt [] "|---|"
            ]
        ]


fRow attrs =
    div (class "flex-row" :: attrs)


fCol attrs =
    div (class "flex-column" :: attrs)


txt =
    textCentered


textCentered attrs string =
    div
        (style "display" "grid"
            :: style "place-content" "center"
            :: style "text-align" "center"
            :: attrs
        )
        [ text string ]


col =
    gridAutoRow


gridAutoRow attrs =
    div
        (style "display" "grid"
            :: style "grid-auto-flow" "row"
            :: attrs
        )


gRow =
    gridAutoCols1fr


gridAutoCols1fr attrs =
    div
        (style "display" "grid"
            :: style "grid-auto-flow" "column"
            :: style "grid-auto-columns" "1fr"
            :: attrs
        )



--noinspection ElmUnusedSymbol


maxW =
    style "max-width"
