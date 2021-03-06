module MontyHallDilemma exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Time
import Utils exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--noinspection ElmUnusedSymbol


width =
    gw * cz



--noinspection ElmUnusedSymbol


height =
    gh * cz


gw =
    3


gh =
    4


cz =
    160



--noinspection ElmUnusedSymbol


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
    | Nop


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )

        DoorClicked doorNum ->
            ( case doorClickedToSimMessage model.sim doorNum of
                Just sMsg ->
                    { model | sim = updateSim sMsg model.sim }

                Nothing ->
                    model
            , Cmd.none
            )

        Nop ->
            ( model, Cmd.none )


doorClickedToSimMessage : Sim -> Int -> Maybe SimMsg
doorClickedToSimMessage sim d =
    case sim.phase of
        AllClosed ->
            Just <| InitialDoorSelected d

        Selected1 _ ->
            Just RevealFirstSheep

        SheepRevealed { ps, hs } ->
            if d == ps then
                Just PlayerSticksToSelection

            else if d /= hs then
                Just PlayerSwapsSelection

            else
                Nothing

        Selected2 _ ->
            Just OpenAllDoors

        AllOpen _ ->
            Nothing


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
    div [ ffMonospace, fontSize "20px", pa "20px" ]
        [ viewSim sim
        , viewAllEmulatedSimStates |> Html.map (always Nop)
        , viewGameResults |> Html.map (always Nop)
        ]


type alias Sim =
    { car : Int, phase : SimPhase }


type SimPhase
    = AllClosed
    | Selected1 { ps : Int }
    | SheepRevealed { ps : Int, hs : Int }
    | Selected2 { ps : Int, hs : Int, ps2 : Int }
    | AllOpen { ps : Int, hs : Int, ps2 : Int }


type SimMsg
    = InitialDoorSelected Int
    | RevealFirstSheep
    | PlayerSticksToSelection
    | PlayerSwapsSelection
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

                hs =
                    [ 2, 3 ]
                        |> List.filter (isCarOrSelected >> not)
                        |> List.head
                        |> Maybe.withDefault 1
            in
            { sim
                | phase =
                    SheepRevealed
                        { ps = ps
                        , hs = hs
                        }
            }

        ( PlayerSticksToSelection, SheepRevealed { ps, hs } ) ->
            { sim
                | phase = Selected2 { ps = ps, hs = hs, ps2 = ps }
            }

        ( PlayerSwapsSelection, SheepRevealed { ps, hs } ) ->
            let
                isSelectedOrRevealed : Int -> Bool
                isSelectedOrRevealed i =
                    i == ps || i == hs

                ps2 =
                    [ 2, 3 ]
                        |> List.filter (isSelectedOrRevealed >> not)
                        |> List.head
                        |> Maybe.withDefault 1
            in
            { sim
                | phase = Selected2 { ps = ps, hs = hs, ps2 = ps2 }
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
        , simToDoorsViewModel sim |> viewDoors
        ]


viewDoors : List DoorView -> Html Msg
viewDoors doors =
    div [ dFlex, contentCenter, itemsCenter ]
        (doors
            |> List.indexedMap
                (\i door ->
                    div
                        [ onClick (DoorClicked <| i + 1)
                        , pa "10px"
                        , dFlex
                        , fDCol
                        , gap "10px"
                        ]
                        [ div [] [ viewDoorContent door ]
                        , div [] [ viewDoorMarker door ]
                        ]
                )
            |> List.intersperse (div [] [ text " | " ])
        )


viewDoorContent : DoorView -> Html msg
viewDoorContent door =
    text <|
        case door.content of
            Just a ->
                case a of
                    SheepContent ->
                        "ss"

                    CarContent ->
                        "cc"

            Nothing ->
                "--"


viewDoorMarker : DoorView -> Html msg
viewDoorMarker door =
    text <|
        case door.marker of
            Just a ->
                case a of
                    PlayerMarker ->
                        "PP"

                    HostMarker ->
                        "HH"

            Nothing ->
                "--"


type alias DoorView =
    { content : Maybe DoorContent
    , marker : Maybe Marker
    }


closedDoor : DoorView
closedDoor =
    DoorView Nothing Nothing


openedDoorWithHostMarker : DoorView
openedDoorWithHostMarker =
    openedDoorWithSheep |> withHostMarker


openedDoorWithCar : DoorView
openedDoorWithCar =
    DoorView (Just CarContent) Nothing


openedDoorWithSheep : DoorView
openedDoorWithSheep =
    DoorView (Just SheepContent) Nothing


closedDoorWithPlayerMarker : DoorView
closedDoorWithPlayerMarker =
    closedDoor |> withPlayerMarker


withPlayerMarker : DoorView -> DoorView
withPlayerMarker dvm =
    { dvm | marker = Just PlayerMarker }


withHostMarker : DoorView -> DoorView
withHostMarker dvm =
    { dvm | marker = Just HostMarker }


type Marker
    = PlayerMarker
    | HostMarker


type DoorContent
    = SheepContent
    | CarContent


simToDoorsViewModel : Sim -> List DoorView
simToDoorsViewModel sim =
    case sim.phase of
        AllClosed ->
            List.repeat 3 closedDoor

        Selected1 { ps } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps then
                            closedDoorWithPlayerMarker

                        else
                            closedDoor
                    )

        SheepRevealed { ps, hs } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps then
                            closedDoorWithPlayerMarker

                        else if i == hs then
                            openedDoorWithHostMarker

                        else
                            closedDoor
                    )

        Selected2 { hs, ps2 } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == ps2 then
                            closedDoorWithPlayerMarker

                        else if i == hs then
                            openedDoorWithHostMarker

                        else
                            closedDoor
                    )

        AllOpen { hs, ps2 } ->
            List.range 1 3
                |> List.map
                    (\i ->
                        if i == sim.car then
                            openedDoorWithCar
                                |> (if i == ps2 then
                                        withPlayerMarker

                                    else
                                        identity
                                   )

                        else
                            openedDoorWithSheep
                                |> (if i == hs then
                                        withHostMarker

                                    else
                                        identity
                                   )
                    )


viewAllEmulatedSimStates : Html Msg
viewAllEmulatedSimStates =
    let
        fn msg ( sim, acc ) =
            ( updateSim msg sim, sim :: acc )

        foo : Sim -> List Sim
        foo sim =
            [ InitialDoorSelected 2
            , RevealFirstSheep
            , PlayerSwapsSelection
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
        , pa "20px"
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
