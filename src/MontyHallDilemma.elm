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
import Tuple exposing (first, pair, second)
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
    { sim : Sim }


init : () -> ( Model, Cmd Msg )
init () =
    ( { sim =
            Random.step randomSim (Random.initialSeed 0)
                |> first
      }
    , Cmd.none
    )


type Msg
    = OnTick
    | SimMsgReceived SimMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )

        SimMsgReceived simMsg ->
            ( { model | sim = updateSim simMsg model.sim }, Cmd.none )


type alias GameData =
    { swapState : SwapState
    , car : Int
    , selection : Int
    }


type SwapState
    = Stick
    | SwappedFrom Int


didWin : GameData -> Bool
didWin { car, selection } =
    car == selection


randomGameStick : Generator GameData
randomGameStick =
    Random.map2 (GameData Stick)
        (Random.uniform 1 [ 2, 3 ])
        (Random.uniform 1 [ 2, 3 ])


randomGameSwap : Generator GameData
randomGameSwap =
    randomGameStick
        |> Random.map revealAndSwapSelection


getRevealedDoor : GameData -> Int
getRevealedDoor game =
    let
        isCarOrSelected i =
            i == game.car || i == game.selection
    in
    [ 2, 3 ]
        |> List.filter (isCarOrSelected >> not)
        |> List.head
        |> Maybe.withDefault 1


revealAndSwapSelection : GameData -> GameData
revealAndSwapSelection game =
    let
        revealedDoor =
            getRevealedDoor game

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
        [ viewSim4
        , viewSim3
        , viewSim2
        , viewSim1
        ]


type SimPhase
    = Initial
    | PlayerMadeInitialSelection
    | HostRevealedSheep
    | PlayerMadeSecondSelection
    | End


type alias Sim =
    { c : Int, p : SimPhase, g : GameData }


type SimMsg
    = InitialDoorSelected Int
    | RevealFirstSheep
    | PlayerSticksToSelection
    | PlayerSwapsSection
    | OpenAllDoors


updateSim : SimMsg -> Sim -> Sim
updateSim msg ({ g, p } as sim) =
    case ( msg, p ) of
        ( InitialDoorSelected d, Initial ) ->
            { sim
                | g = { g | selection = clamp 1 3 d }
                , p = PlayerMadeInitialSelection
            }

        ( RevealFirstSheep, PlayerMadeInitialSelection ) ->
            { sim
                | p = HostRevealedSheep
            }

        ( PlayerSticksToSelection, HostRevealedSheep ) ->
            { sim
                | p = PlayerMadeSecondSelection
            }

        ( PlayerSwapsSection, HostRevealedSheep ) ->
            { sim
                | g = revealAndSwapSelection g
                , p = PlayerMadeSecondSelection
            }

        ( OpenAllDoors, PlayerMadeSecondSelection ) ->
            { sim
                | p = End
            }

        _ ->
            sim


randomSim : Generator Sim
randomSim =
    randomGameStick
        |> Random.map (\g -> { c = g.car, g = g, p = Initial })


viewSim : Sim -> Html Msg
viewSim ({ g, p } as sim) =
    div []
        [ div [] [ text <| Debug.toString p ]
        , div [] [ text <| Debug.toString g ]
        , div []
            (simToDoorsViewModel sim
                |> String.join " | "
                |> text
                |> List.singleton
            )
        ]


simToDoorsViewModel : Sim -> List String
simToDoorsViewModel sim =
    case sim.p of
        Initial ->
            [ closedDoor, closedDoor, closedDoor ]

        PlayerMadeInitialSelection ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == sim.g.selection then
                            selectedDoor

                        else
                            closedDoor
                    )

        HostRevealedSheep ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == sim.g.selection then
                            selectedDoor

                        else if i == getRevealedDoor sim.g then
                            sheepRevealed

                        else
                            closedDoor
                    )

        PlayerMadeSecondSelection ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == sim.g.selection then
                            selectedDoor

                        else if i == getRevealedDoor sim.g then
                            sheepRevealed

                        else
                            closedDoor
                    )

        End ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == sim.g.car then
                            carRevealed

                        else
                            sheepRevealed
                    )


closedDoor =
    "--"


selectedDoor =
    "PP"


sheepRevealed =
    "ss"


carRevealed =
    "cc"


viewSim4 =
    let
        fn msg ( sim, acc ) =
            ( updateSim msg sim, sim :: acc )

        foo : Sim -> List Sim
        foo sim =
            [ InitialDoorSelected 2
            , RevealFirstSheep
            , PlayerSwapsSection
            , OpenAllDoors
            ]
                |> List.foldl fn ( sim, [] )
                |> (\( x, xs ) -> x :: xs |> List.reverse)
    in
    let
        sim : Sim
        sim =
            Random.step randomSim (Random.initialSeed 0)
                |> first

        sims : List Sim
        sims =
            foo sim
    in
    div
        [ tac
        , dFlex
        , fDCol
        , gap "20px"
        , pAll "20px"
        ]
        (List.map viewSim sims)


viewSim3 =
    let
        sim : Sim
        sim =
            Random.step randomSim (Random.initialSeed 0)
                |> first

        sims : List Sim
        sims =
            [ Initial
            , PlayerMadeInitialSelection
            , HostRevealedSheep
            , PlayerMadeSecondSelection
            , End
            ]
                |> List.map
                    (\p ->
                        case p of
                            PlayerMadeSecondSelection ->
                                { sim | g = revealAndSwapSelection sim.g, p = p }

                            _ ->
                                { sim | p = p }
                    )
    in
    div
        [ tac
        , dFlex
        , fDCol
        , gap "20px"
        , pAll "20px"
        ]
        (List.map viewSim sims)


viewSim2 =
    let
        sims =
            [ ( "--|--|--", "Player: Select Door" )
            , ( "PP|--|--", "Host: Select Sheep to Reveal" )
            , ( "PP|sh|--", "Player: Stick or Swap" )
            , ( "--|sh|PP", "Host: Open All" )
            , ( "cc|sh|sh", "==Over==" )
            ]

        vs ( state, title ) =
            div []
                [ div [] [ text title ]
                , div [] [ text state ]
                ]
    in
    div
        [ tac
        , dFlex
        , fDCol
        , gap "20px"
        , pAll "20px"
        ]
        (List.map vs sims)


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


viewSummary : List GameData -> Html msg
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


viewGame : GameData -> Html msg
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
