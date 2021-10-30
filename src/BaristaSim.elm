module BaristaSim exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href, rel, style)
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
    | HH_MilkCarton
    | HH_MilkJar


type CoffeeCup
    = CoffeeCupEmpty
    | CoffeeCupWithEspresso


type DesktopHolder
    = DH_CoffeeCup CoffeeCup


type alias CoffeeMaker =
    ( Maybe Strainer, Maybe CoffeeCup )


type alias Model =
    { strainerHolderA : Maybe Strainer
    , coffeePowderDispenser : Maybe Strainer
    , coffeeMaker : CoffeeMaker
    , hands : Maybe HandHoldable
    , desktopHolders : Dict Int DesktopHolder
    }


init : Model
init =
    { strainerHolderA = Just StrainerEmpty
    , coffeePowderDispenser = Nothing
    , coffeeMaker = ( Nothing, Nothing )
    , hands = Nothing
    , desktopHolders = Dict.empty
    }


type Msg
    = StrainerHolderAClicked
    | CoffeePowderDispenserClicked
    | CoffeeMakerStrainerHolderClicked
    | CoffeeMakerCupHolderClicked
    | TrashClicked
    | CoffeeCupStackClicked
    | DesktopHolderClicked Int


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


tryToMakeCoffee : CoffeeMaker -> CoffeeMaker
tryToMakeCoffee coffeeMaker =
    case coffeeMaker of
        ( Just StrainerWithCoffeePowder, Just CoffeeCupEmpty ) ->
            ( Just StrainerWithWaste, Just CoffeeCupWithEspresso )

        _ ->
            coffeeMaker


view : Model -> Html Msg
view model =
    div [ class "debug" ]
        [ Html.node "link" [ rel "stylesheet", href "barista.css" ] []
        , div
            [ style "padding" "1.5rem"
            , class "flex-column gap1"
            ]
            [ div
                [ onClick StrainerHolderAClicked
                , class "flex-row gap1"
                ]
                [ divText [] "strainerHolderA"
                , divText [] (Debug.toString model.strainerHolderA)
                ]
            , div [ onClick CoffeeCupStackClicked, class "flex-row gap1" ]
                [ divText [] "Coffee Cups Stack"
                ]
            , div [ onClick CoffeePowderDispenserClicked, class "flex-row gap1" ]
                [ divText [] "coffeePowderDispenser"
                , divText [] (Debug.toString model.coffeePowderDispenser)
                ]
            , div [ onClick CoffeeMakerStrainerHolderClicked, class "flex-row gap1" ]
                [ divText [] "coffeeMakerStrainerHolder"
                , divText [] (Debug.toString (Tuple.first model.coffeeMaker))
                ]
            , div [ onClick CoffeeMakerCupHolderClicked, class "flex-row gap1" ]
                [ divText [] "coffeeMakerCupHolder"
                , divText [] (Debug.toString (Tuple.second model.coffeeMaker))
                ]
            , div [ onClick TrashClicked, class "flex-row gap1" ]
                [ divText [] "Trash"
                ]
            , div [ class "flex-row gap1" ]
                [ divText [] "Hands"
                , divText [] (Debug.toString model.hands)
                ]
            , div
                [ class "flex-row gap1" ]
                (List.range 0 5
                    |> List.map
                        (\i ->
                            divText
                                [ style "flex" "1 1 auto"
                                , style "text-align" "center"
                                , onClick (DesktopHolderClicked i)
                                ]
                                (Debug.toString (i + 1))
                        )
                )
            ]
        ]


divText attrs string =
    div attrs [ text string ]
