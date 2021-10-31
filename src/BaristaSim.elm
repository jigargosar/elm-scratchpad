module BaristaSim exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href, rel, style, title)
import Html.Events exposing (onClick)


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
    div [ class "debug" ]
        [ Html.node "link" [ rel "stylesheet", href "barista.css" ] []
        , div
            [ style "padding" "1.5rem"
            , class "flex-column gap1"
            ]
            [ div [ class "flex-row gap1" ]
                (List.range 0 2
                    |> List.map
                        (\i ->
                            div
                                [ class "flex-column gap1"
                                , style "flex" "1 0 0"
                                , onClick (CheckoutHolderClicked i)
                                ]
                                [ textEl [] (Debug.toString (i + 1))
                                , textEl [] (Debug.toString (Dict.get i model.orders))
                                , textEl [] (Debug.toString (Dict.get i model.checkoutHolders))
                                ]
                        )
                )
            , div [ onClick StrainerHolderAClicked, class "flex-row gap1" ]
                [ textEl [] "strainerHolderA"
                , textEl [] (Debug.toString model.strainerHolderA)
                ]
            , div [ onClick CoffeeCupStackClicked, class "flex-row gap1" ]
                [ textEl [] "Coffee Cups Stack"
                ]
            , div
                [ style "display" "grid"
                , style "grid-auto-flow" "column"
                , class "gap1"

                --, style "padding" "1.5rem"
                ]
                [ viewEspressoMaker model.coffeeMaker
                , viewCoffeePowderDispenser model.coffeePowderDispenser
                ]

            --, div [ onClick CoffeeMakerStrainerHolderClicked, class "flex-row gap1" ]
            --    [ divText [] "coffeeMakerStrainerHolder"
            --    , divText [] (Debug.toString (Tuple.first model.coffeeMaker))
            --    ]
            --, div [ onClick CoffeeMakerCupHolderClicked, class "flex-row gap1" ]
            --    [ divText [] "coffeeMakerCupHolder"
            --    , divText [] (Debug.toString (Tuple.second model.coffeeMaker))
            --    ]
            , div [ onClick TrashClicked, class "flex-row gap1" ]
                [ textEl [] "Trash"
                ]
            , div [ class "flex-row gap1" ]
                [ textEl [] "Hands"
                , textEl [] (Debug.toString model.hands)
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
                                [ textEl [] (Debug.toString (i + 1))
                                , textEl [] (Debug.toString (Dict.get i model.desktopHolders))
                                ]
                        )
                )
            ]
        ]


viewCoffeePowderDispenser : Maybe Strainer -> Html Msg
viewCoffeePowderDispenser mbStrainer =
    gCol
        [ class "tac debug"
        , title <| Debug.toString mbStrainer
        , onClick CoffeePowderDispenserClicked
        ]
        [ textEl [] "Coffee Dispenser"
        , textEl []
            (case mbStrainer of
                Just _ ->
                    "Strainer"

                Nothing ->
                    "|---|"
            )
        ]


viewEspressoMaker : EspressoMaker -> Html Msg
viewEspressoMaker ( mbStrainer, mbCup ) =
    fCol [ class "tac" ]
        [ textEl [] "Espresso Maker"
        , textEl
            [ onClick CoffeeMakerStrainerHolderClicked
            , title (Debug.toString mbStrainer)
            ]
            (case mbStrainer of
                Just _ ->
                    "Strainer"

                Nothing ->
                    "|---|"
            )
        , fRow [ onClick CoffeeMakerCupHolderClicked ]
            [ textEl
                [ class "flex100"
                , title (Debug.toString mbCup)
                ]
                (case mbCup of
                    Just _ ->
                        "Cup"

                    Nothing ->
                        "|---|"
                )
            , div [ class "flex100" ] [ text "|---|" ]
            ]
        ]


textEl attrs string =
    div attrs [ text string ]


fRow attrs =
    div (class "flex-row" :: attrs)


fCol attrs =
    div (class "flex-column" :: attrs)


gCol attrs =
    div (style "display" "grid" :: style "grid-auto-flow" "row" :: attrs)



--noinspection ElmUnusedSymbol


maxW =
    style "max-width"
