module MontyHallDilemma exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Time
import Tuple exposing (first)
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
import TypedSvg.Types as TT
import Utils exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


width =
    gw * cz


height =
    gh * cz


gw =
    3


gh =
    4


cz =
    160


gpToWorld : GPos -> Vec
gpToWorld =
    vFromGP >> vScale cz >> vAdd1 (cz / 2)


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}
    , Cmd.none
    )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )


type alias GameResult =
    { swapState : SwapState
    , car : Int
    , selection : Int
    }


type SwapState
    = NoSwap
    | SwappedFrom Int


didWin : GameResult -> Bool
didWin { car, selection } =
    car == selection


randomGameStick : Generator GameResult
randomGameStick =
    Random.map2 (GameResult NoSwap)
        (Random.uniform 1 [ 2, 3 ])
        (Random.uniform 1 [ 2, 3 ])


randomGameSwap : Generator GameResult
randomGameSwap =
    randomGameStick
        |> Random.map revealAndSwapSelection


revealAndSwapSelection : GameResult -> GameResult
revealAndSwapSelection game =
    let
        isCarOrSelected i =
            i == game.car || i == game.selection

        revealedDoor : Int
        revealedDoor =
            [ 2, 3 ]
                |> List.filter (isCarOrSelected >> not)
                |> List.head
                |> Maybe.withDefault 1

        isSelectedOrRevealed : Int -> Bool
        isSelectedOrRevealed i =
            i == game.selection || i == revealedDoor

        newSelection =
            List.filter (isSelectedOrRevealed >> not) [ 2, 3 ]
                |> List.head
                |> Maybe.withDefault 1
    in
    { game | selection = newSelection, swapState = SwappedFrom game.selection }


view : Model -> Html Msg
view _ =
    div [ fontMono, fontSize "20px" ]
        [ viewSim3
        , viewSim2
        , viewSim1
        ]


viewSim3 =
    let
        sims =
            [ ( "--|--|--", "Select Door" )
            , ( "PP|--|--", "Host: Select Sheep to Reveal" )
            , ( "PP|sh|--", "First Sheep Revealed" )
            , ( "--|sh|PP", "Swap Selection" )
            , ( "cc|sh|sh", "Final Reveal" )
            ]

        vs ( state, title ) =
            div [] [ text state, text " : ", text title ]
    in
    sims |> List.map vs |> div []


viewSim2 =
    let
        sims =
            [ "1: Select Door"
            , "2: Reveal Sheep"
            , "3.1: Stick"
            , "3.2: Swap"
            , "4: Final Reveal"
            ]

        vs t =
            div [] [ text t ]
    in
    sims |> List.map vs |> div []


viewSim1 =
    let
        games =
            Random.step (Random.list maxGames randomGameSwap)
                (Random.initialSeed 0)
                |> first

        maxGames =
            1000
    in
    div []
        [ viewSummary games
        , div [] (games |> List.map viewGame)
        ]


viewSummary : List GameResult -> Html msg
viewSummary games =
    let
        total =
            List.length games

        won =
            List.filter didWin games |> List.length

        winPct =
            (toFloat won / toFloat total * 100)
                |> mul 100
                |> round
                |> toFloat
                |> mul 0.01
    in
    div []
        [ [ "Won: " ++ String.fromInt won
          , "Total: " ++ String.fromInt total
          , "Win Pct: " ++ String.fromFloat winPct ++ "%"
          ]
            |> String.join " "
            |> text
        ]


viewGame : GameResult -> Html msg
viewGame game =
    let
        txt =
            if didWin game then
                "Win"

            else
                "Loss"
    in
    div []
        [ div []
            [ Debug.toString game |> text
            , text ": "
            , text txt
            ]
        ]



--view : Model -> Html Msg
--view model =
--    Svg.svg
--        [ saWidth width
--        , saHeight height
--        , noFill
--        , noStroke
--        , bgc gray
--        , noUserSelect
--        ]
--        []
