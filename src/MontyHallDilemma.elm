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
        , viewSim2
        , viewSim1
        ]


type alias Sim =
    { car : Int, phase : SimPhase }


type SimPhase
    = Initial
    | FirstSelection { ps : Int }
    | SheepRevealed { ps : Int, rs : Int }
    | SecondSelection { ps : Int, rs : Int, ps2 : Int }
    | End { ps : Int, rs : Int, ps2 : Int }


type SimMsg
    = InitialDoorSelected Int
    | RevealFirstSheep
    | PlayerSticksToSelection
    | PlayerSwapsSection
    | OpenAllDoors


updateSim : SimMsg -> Sim -> Sim
updateSim msg ({ car, phase } as sim) =
    case ( msg, phase ) of
        ( InitialDoorSelected d, Initial ) ->
            if clamp 1 3 d /= d then
                sim

            else
                { sim
                    | phase = FirstSelection { ps = clamp 1 3 d }
                }

        ( RevealFirstSheep, FirstSelection { ps } ) ->
            let
                isCarOrSelected i =
                    i == car || i == ps

                rs =
                    [ 2, 3 ]
                        |> List.filter (isCarOrSelected >> not)
                        |> List.head
                        |> Maybe.withDefault 1
            in
            { sim
                | phase =
                    SheepRevealed
                        { ps = ps
                        , rs = rs
                        }
            }

        ( PlayerSticksToSelection, SheepRevealed { ps, rs } ) ->
            { sim
                | phase = SecondSelection { ps = ps, rs = rs, ps2 = ps }
            }

        ( PlayerSwapsSection, SheepRevealed { ps, rs } ) ->
            let
                isSelectedOrRevealed : Int -> Bool
                isSelectedOrRevealed i =
                    i == ps || i == rs

                ps2 =
                    [ 2, 3 ]
                        |> List.filter (isSelectedOrRevealed >> not)
                        |> List.head
                        |> Maybe.withDefault 1
            in
            { sim
                | phase = SecondSelection { ps = ps, rs = rs, ps2 = ps2 }
            }

        ( OpenAllDoors, SecondSelection rec ) ->
            { sim
                | phase = End rec
            }

        _ ->
            sim


randomSim : Generator Sim
randomSim =
    Random.uniform 1 [ 2, 3 ]
        |> Random.map (\car -> { car = car, phase = Initial })


viewSim : Sim -> Html Msg
viewSim sim =
    div []
        [ div [] [ text <| Debug.toString sim.phase ]
        , div []
            (simToDoorsViewModel sim
                |> List.map text
                |> List.intersperse (text " | ")
            )
        ]


simToDoorsViewModel : Sim -> List String
simToDoorsViewModel sim =
    case sim.phase of
        Initial ->
            [ closedDoor, closedDoor, closedDoor ]

        FirstSelection { ps } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps then
                            selectedDoor

                        else
                            closedDoor
                    )

        SheepRevealed { ps, rs } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps then
                            selectedDoor

                        else if i == rs then
                            sheepRevealed

                        else
                            closedDoor
                    )

        SecondSelection { rs, ps2 } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps2 then
                            selectedDoor

                        else if i == rs then
                            sheepRevealed

                        else
                            closedDoor
                    )

        End _ ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == sim.car then
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
            , PlayerSticksToSelection
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
