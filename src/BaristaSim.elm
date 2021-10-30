module BaristaSim exposing (..)

import Browser
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


type Holder
    = StrainerHolder (Maybe Strainer)
    | CoffeePowderDispenser (Maybe Strainer)
    | CoffeeMaker (Maybe Strainer) (Maybe CoffeeCup) (Maybe CoffeeCup)
    | SteamDispenser
    | Trash
    | CoffeeCupStack


type alias Model =
    { strainerHolderA : Maybe Strainer
    , coffeePowderDispenser : Maybe Strainer
    , coffeeMaker : ( Maybe Strainer, Maybe CoffeeCup )
    , hands : Maybe HandHoldable
    }


init : Model
init =
    { strainerHolderA = Just StrainerEmpty
    , coffeePowderDispenser = Nothing
    , coffeeMaker = ( Nothing, Nothing )
    , hands = Nothing
    }


type Msg
    = StrainerHolderAClicked
    | CoffeePowderDispenserClicked
    | CoffeeMakerClicked
    | TrashClicked
    | CoffeeCupStackClicked


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

                _ ->
                    model

        CoffeeMakerClicked ->
            case ( model.hands, model.coffeeMaker ) of
                ( Just (HH_Strainer StrainerWithCoffeePowder), ( Nothing, cup ) ) ->
                    { model | hands = Nothing, coffeeMaker = ( Just StrainerWithCoffeePowder, cup ) }

                _ ->
                    model

        CoffeeCupStackClicked ->
            case model.hands of
                Nothing ->
                    { model | hands = Just (HH_CoffeeCup CoffeeCupEmpty) }

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
            , div [ onClick CoffeeMakerClicked, class "flex-row gap1" ]
                [ divText [] "coffeeMaker"
                , divText [] (Debug.toString model.coffeeMaker)
                ]
            , div [ onClick TrashClicked, class "flex-row gap1" ]
                [ divText [] "Trash"
                ]
            , div [ class "flex-row gap1" ]
                [ divText [] "Hands"
                , divText [] (Debug.toString model.hands)
                ]
            ]
        ]


divText attrs string =
    div attrs [ text string ]
