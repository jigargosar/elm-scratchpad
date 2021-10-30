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
    | HH_CoffeeCup
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
    , hands : Maybe HandHoldable
    }


init : Model
init =
    Model (Just StrainerEmpty) Nothing Nothing


type Msg
    = StrainerHolderAClicked
    | CoffeePowderDispenserClicked
    | TrashClicked


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
            , div
                [ onClick CoffeePowderDispenserClicked
                , class "flex-row gap1"
                ]
                [ divText [] "coffeePowderDispenser"
                , divText [] (Debug.toString model.coffeePowderDispenser)
                ]
            , div
                [ onClick TrashClicked
                , class "flex-row gap1"
                ]
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
