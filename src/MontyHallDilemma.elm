module MontyHallDilemma exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
    | DoorClicked Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )

        DoorClicked d ->
            let
                simMsg =
                    doorClickedToSimMessage model.sim d
            in
            ( { model
                | sim = updateSim simMsg model.sim
              }
            , Cmd.none
            )


doorClickedToSimMessage : Sim -> Int -> SimMsg
doorClickedToSimMessage sim d =
    case sim.phase of
        AllClosed ->
            InitialDoorSelected d

        Selected1 _ ->
            RevealFirstSheep

        SheepRevealed { ps, rs } ->
            if d == ps then
                PlayerSticksToSelection

            else if d /= rs then
                PlayerSwapsSection

            else
                InitialDoorSelected d

        Selected2 _ ->
            OpenAllDoors

        AllOpen _ ->
            InitialDoorSelected d


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
view { sim } =
    div [ fontMono, fontSize "20px" ]
        [ viewDoors (simToDoorsViewModel sim)
        , viewSim sim
        , viewSim4
        , viewGameResults
        ]


type alias Sim =
    { car : Int, phase : SimPhase }


type SimPhase
    = AllClosed
    | Selected1 { ps : Int }
    | SheepRevealed { ps : Int, rs : Int }
    | Selected2 { ps : Int, rs : Int, ps2 : Int }
    | AllOpen { ps : Int, rs : Int, ps2 : Int }


type SimMsg
    = InitialDoorSelected Int
    | RevealFirstSheep
    | PlayerSticksToSelection
    | PlayerSwapsSection
    | OpenAllDoors


updateSim : SimMsg -> Sim -> Sim
updateSim msg ({ car, phase } as sim) =
    case ( msg, phase ) of
        ( InitialDoorSelected d, AllClosed ) ->
            if clamp 1 3 d /= d then
                sim

            else
                { sim
                    | phase = Selected1 { ps = clamp 1 3 d }
                }

        ( RevealFirstSheep, Selected1 { ps } ) ->
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
                | phase = Selected2 { ps = ps, rs = rs, ps2 = ps }
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
                | phase = Selected2 { ps = ps, rs = rs, ps2 = ps2 }
            }

        ( OpenAllDoors, Selected2 rec ) ->
            { sim
                | phase = AllOpen rec
            }

        _ ->
            sim


randomSim : Generator Sim
randomSim =
    Random.uniform 1 [ 2, 3 ]
        |> Random.map (\car -> { car = car, phase = AllClosed })


viewSim : Sim -> Html Msg
viewSim sim =
    div []
        [ div [ tac ] [ text <| Debug.toString sim.phase ]
        , div [ dFlex, contentCenter, itemsCenter ]
            (simToDoorsStringViewModel sim
                |> List.indexedMap
                    (\i string ->
                        div
                            [ onClick (DoorClicked <| i + 1)
                            , pAll "10px"
                            ]
                            [ text string ]
                    )
                |> List.intersperse (div [] [ text " | " ])
            )
        ]


viewDoors : List DoorView -> Html Msg
viewDoors doors =
    div [ dFlex, contentCenter, itemsCenter ]
        (doors
            |> List.indexedMap
                (\i door ->
                    div
                        [ onClick (DoorClicked <| i + 1)
                        , pAll "10px"
                        ]
                        [ viewDoor door ]
                )
            |> List.intersperse (div [] [ text " | " ])
        )


viewDoor : DoorView -> Html msg
viewDoor door =
    text <|
        case door of
            Closed ->
                "--"

            Sheep ->
                "ss"

            Car ->
                "cc"

            Selected ->
                "PP"


type DoorView
    = Closed
    | Sheep
    | Car
    | Selected


simToDoorsViewModel : Sim -> List DoorView
simToDoorsViewModel sim =
    case sim.phase of
        AllClosed ->
            [ Closed, Closed, Closed ]

        Selected1 { ps } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps then
                            Selected

                        else
                            Closed
                    )

        SheepRevealed { ps, rs } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps then
                            Selected

                        else if i == rs then
                            Sheep

                        else
                            Closed
                    )

        Selected2 { rs, ps2 } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps2 then
                            Selected

                        else if i == rs then
                            Sheep

                        else
                            Closed
                    )

        AllOpen _ ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == sim.car then
                            Car

                        else
                            Sheep
                    )


simToDoorsStringViewModel : Sim -> List String
simToDoorsStringViewModel sim =
    case sim.phase of
        AllClosed ->
            [ closedDoor, closedDoor, closedDoor ]

        Selected1 { ps } ->
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

        Selected2 { rs, ps2 } ->
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

        AllOpen _ ->
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


viewGameResults : Html msg
viewGameResults =
    let
        games : List GameData
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
