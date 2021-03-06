module BaristaSim exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, text)
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
    , espressoMaker : EspressoMaker
    , hands : Maybe HandHoldable
    , desktopHolders : Dict Int DesktopHolder
    , checkoutHolders : Dict Int CheckoutHolder
    , orders : Dict Int Order
    }


init : Model
init =
    { strainerHolderA = Just StrainerEmpty
    , coffeePowderDispenser = Nothing
    , espressoMaker = ( Nothing, Just CoffeeCupWithEspresso )
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
            case ( model.hands, model.espressoMaker ) of
                ( Just (HH_Strainer StrainerWithCoffeePowder), ( Nothing, cup ) ) ->
                    { model
                        | hands = Nothing
                        , espressoMaker = ( Just StrainerWithCoffeePowder, cup ) |> tryToMakeCoffee
                    }

                ( Nothing, ( Just (StrainerWithWaste as strainer), cup ) ) ->
                    { model
                        | hands = Just (HH_Strainer strainer)
                        , espressoMaker = ( Nothing, cup ) |> tryToMakeCoffee
                    }

                _ ->
                    model

        CoffeeMakerCupHolderClicked ->
            case ( model.hands, model.espressoMaker ) of
                ( Just (HH_CoffeeCup cup), ( strainer, Nothing ) ) ->
                    { model
                        | hands = Nothing
                        , espressoMaker =
                            ( strainer, Just cup )
                                |> tryToMakeCoffee
                    }

                ( Nothing, ( strainer, Just cup ) ) ->
                    { model | hands = Just (HH_CoffeeCup cup), espressoMaker = ( strainer, Nothing ) }

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
            [ gRow [ class "" ]
                (List.range 0 2
                    |> List.map
                        (\i ->
                            col
                                [ class "gap05"
                                , onClick (CheckoutHolderClicked i)
                                ]
                                [ txt [] (Debug.toString (Dict.get i model.orders))
                                , let
                                    ch =
                                        Dict.get i model.checkoutHolders

                                    debugTitle =
                                        title <| Debug.toString ch
                                  in
                                  case ch of
                                    Nothing ->
                                        viewMaybeCoffeeCup [ debugTitle ] Nothing

                                    Just (CH_CoffeeCup cc) ->
                                        viewMaybeCoffeeCup [ debugTitle ] (Just cc)
                                ]
                        )
                )
            , fRowWrap [ onClick CoffeeCupStackClicked, contentCentered ]
                (List.repeat 4 (viewMaybeCoffeeCup [] (Just CoffeeCupEmpty)))
            , gRow
                [ class "gap1"
                ]
                [ viewEspressoMaker model.espressoMaker
                , viewCoffeePowderDispenser model.coffeePowderDispenser
                , viewMaybeStrainer
                    [ onClick StrainerHolderAClicked
                    , title <| Debug.toString model.strainerHolderA
                    ]
                    model.strainerHolderA
                ]
            , gRow []
                [ col [ class "", title (Debug.toString model.hands) ]
                    [ txt [] "Hands"
                    , case model.hands of
                        Nothing ->
                            viewMaybeCoffeeCup [] Nothing

                        Just (HH_CoffeeCup cc) ->
                            viewMaybeCoffeeCup [] (Just cc)

                        Just (HH_Strainer s) ->
                            viewMaybeStrainer [] (Just s)
                    ]
                , txt [ onClick TrashClicked ] "Trash"
                ]
            , gRow []
                (List.range 0 3 |> List.map (viewDesktopHolder model.desktopHolders))
            , gRow []
                (List.range 3 6 |> List.map (viewDesktopHolder model.desktopHolders))
            ]
        ]


viewDesktopHolder : Dict Int DesktopHolder -> Int -> Html Msg
viewDesktopHolder dict i =
    let
        mbDH =
            Dict.get i dict

        debugTitle =
            title (Debug.toString mbDH)

        clickAttr =
            onClick (DesktopHolderClicked i)

        attrs =
            [ debugTitle, clickAttr ]
    in
    case mbDH of
        Nothing ->
            viewMaybeCoffeeCup attrs <| Nothing

        Just (DH_CoffeeCup cc) ->
            viewMaybeCoffeeCup attrs <| Just cc


viewCoffeePowderDispenser : Maybe Strainer -> Html Msg
viewCoffeePowderDispenser mbStrainer =
    col
        [ class "debug"

        --, style "height" "200px"
        , title <| Debug.toString mbStrainer
        , onClick CoffeePowderDispenserClicked
        ]
        [ txt [] "Coffee Dispenser"
        , viewMaybeStrainer [] mbStrainer
        ]


viewMaybeCoffeeCup : List (Attribute msg) -> Maybe CoffeeCup -> Html msg
viewMaybeCoffeeCup attrs mbCoffeeCup =
    elCentered attrs <|
        case mbCoffeeCup of
            Just cc ->
                drawCoffeeCupShape (coffeeCupToFill cc) []

            Nothing ->
                drawCoffeeCupShape "lightsteelblue" [ SA.opacity "0.2" ]


coffeeCupToFill : CoffeeCup -> String
coffeeCupToFill coffeeCup =
    case coffeeCup of
        CoffeeCupEmpty ->
            "lightsteelblue"

        CoffeeCupWithEspresso ->
            "brown"


drawCoffeeCupShape : String -> List (Attribute msg) -> Html msg
drawCoffeeCupShape contentFill attrs =
    Svg.svg
        [ viewBox2 64 64
        , Px.width 64
        , SA.fill "none"
        , SA.stroke "none"
        ]
        [ group attrs
            [ rect 16 8 [ SA.fill "#000", TA.transform [ TT.Translate 16 0 ] ]
            , Svg.circle
                [ SA.stroke "#000"
                , Px.strokeWidth 4
                , SA.fill "hsl(0.55turn 70% 50% / 1)"
                , SA.fill contentFill
                , Px.r 16

                --, TA.transform [ TT.Translate -16 0 ]
                ]
                []
            ]
        ]


viewMaybeStrainer attrs mbStrainer =
    elCentered attrs <|
        case mbStrainer of
            Nothing ->
                drawStrainerShape (strainerToContentFill StrainerEmpty) [ SA.opacity "0.1" ]

            Just strainer ->
                drawStrainerShape (strainerToContentFill strainer) []


strainerToContentFill : Strainer -> String
strainerToContentFill strainer =
    case strainer of
        StrainerEmpty ->
            "lightsteelblue"

        StrainerWithCoffeePowder ->
            "brown"

        StrainerWithWaste ->
            "#000"


drawStrainerShape : String -> List (Attribute msg) -> Html msg
drawStrainerShape contentFill attrs =
    Svg.svg
        [ viewBox2 128 64
        , Px.width 128
        , SA.fill "none"
        , SA.stroke "none"

        --, SA.class "debug"
        ]
        [ group attrs
            [ Svg.circle
                [ SA.stroke "#000"
                , Px.strokeWidth 4
                , SA.fill "hsl(0.55turn 70% 50% / 1)"
                , SA.fill contentFill
                , Px.r 16
                , TA.transform [ TT.Translate -16 0 ]
                ]
                []
            , rect 32 16 [ SA.fill "#000", TA.transform [ TT.Translate 16 0 ] ]
            ]
        ]


viewEspressoMaker : EspressoMaker -> Html Msg
viewEspressoMaker ( mbStrainer, mbCup ) =
    col [ class "debug" ]
        [ txt [] "Espresso Maker"
        , viewMaybeStrainer
            [ onClick CoffeeMakerStrainerHolderClicked
            , title (Debug.toString mbStrainer)
            ]
            mbStrainer
        , gRow []
            [ viewMaybeCoffeeCup
                [ title (Debug.toString mbCup)
                , onClick CoffeeMakerCupHolderClicked
                ]
                mbCup
            , viewMaybeCoffeeCup
                [ title (Debug.toString mbCup)
                , onClick CoffeeMakerCupHolderClicked
                ]
                mbCup
            ]
        ]



-- SVG UTILS


group =
    Svg.g


rect w h xs =
    Svg.rect (Px.x (-w / 2) :: Px.y (-h / 2) :: Px.width w :: Px.height h :: xs) []


viewBox2 w h =
    TA.viewBox (-w / 2) (-h / 2) w h



-- HTML UTILS
--noinspection ElmUnusedSymbol


fRow attrs =
    div
        (style "display" "flex"
            :: style "flex-flow" "row nowrap"
            :: attrs
        )


fRowWrap attrs =
    div
        (style "display" "flex"
            :: style "flex-flow" "row wrap"
            :: attrs
        )



--noinspection ElmUnusedSymbol


fCol attrs =
    div (style "display" "flex" :: style "flex-direction" "column" :: attrs)


txt =
    textCentered


textCentered attrs string =
    elCentered attrs <| text string


elCentered attrs singleElement =
    div
        (style "display" "grid"
            :: contentCentered
            :: style "text-align" "center"
            :: attrs
        )
        [ singleElement ]


col =
    gridAutoFlowRow


gridAutoFlowRow attrs =
    div
        (style "display" "grid"
            :: style "grid-auto-flow" "row"
            :: attrs
        )


gRow =
    gridAutoFlowColumns1fr


gridAutoFlowColumns1fr attrs =
    div
        (style "display" "grid"
            :: style "grid-auto-flow" "column"
            :: style "grid-auto-columns" "1fr"
            :: attrs
        )



--noinspection ElmUnusedSymbol


maxW =
    style "max-width"


contentCentered =
    style "place-content" "center"
