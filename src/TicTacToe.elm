module TicTacToe exposing (main)

import Dict exposing (Dict)
import Html.Events
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { bd : BoardDict }


init : () -> ( Model, Cmd Msg )
init () =
    ( { bd =
            emptyBoardDict
                |> withRollback (makeMove ( 0, 0 ))
                |> withRollback (makeMove ( 1, 1 ))
                |> withRollback (makeMove ( 2, 2 ))
      }
    , Cmd.none
    )


type Msg
    = OnClick GPos


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClick gp ->
            ( updateOnGPClick gp model, Cmd.none )


updateOnGPClick : GPos -> Model -> Model
updateOnGPClick gp ({ bd } as model) =
    { model | bd = withRollback (makeMove gp) bd }


view : Model -> Document Msg
view model =
    Document "Tic Tac Toe - Game"
        [ basicStylesNode
        , viewBoardSvg model.bd
        , div [] [ text <| Debug.toString <| getWinner model.bd ]
        ]


viewBoardSvg : BoardDict -> Html Msg
viewBoardSvg bd =
    svgBlock
        [ sMaxHeight "100vh"
        , viewBoxC 300 300
        , ffMonospace
        , fontSize "12px"
        ]
        [ viewBD bd
        ]


type Mark
    = Cross
    | Zero


type Slot
    = Marked Mark
    | Empty


type alias BoardDict =
    Dict GPos Slot


emptyBoardDict : BoardDict
emptyBoardDict =
    squareGridPositions 3
        |> List.map (pairTo Empty)
        |> Dict.fromList


makeMove : GPos -> BoardDict -> Maybe BoardDict
makeMove gp bd =
    case
        ( getWinner bd
        , Dict.get gp bd
        , getNextMarker bd
        )
    of
        ( Nothing, Just Empty, Just nextMarker ) ->
            Just (Dict.insert gp (Marked nextMarker) bd)

        _ ->
            Nothing


getWinner : BoardDict -> Maybe Mark
getWinner bd =
    let
        gps =
            squareGridPositions 3
    in
    -- columns
    groupEqBy first gps
        -- rows
        ++ groupEqBy second gps
        ++ [ -- diagonal 1
             ( ( 0, 0 ), [ ( 1, 1 ), ( 2, 2 ) ] )

           -- diagonal 2
           , ( ( 2, 0 ), [ ( 1, 1 ), ( 0, 2 ) ] )
           ]
        |> findMapFirst (getWinnerFromConsGPS bd)


getWinnerFromConsGPS : BoardDict -> ( GPos, List GPos ) -> Maybe Mark
getWinnerFromConsGPS bd ( h, t ) =
    getWinnerFromGPS bd (h :: t)


getWinnerFromGPS : BoardDict -> List GPos -> Maybe Mark
getWinnerFromGPS bd tgp =
    case List.filterMap (\k -> Dict.get k bd) tgp of
        ((Marked mark) as h) :: t ->
            if List.all (eq h) t then
                Just mark

            else
                Nothing

        _ ->
            Nothing


getNextMarker : BoardDict -> Maybe Mark
getNextMarker bd =
    let
        emptyCt =
            Dict.filter (\_ -> eq Empty) bd |> Dict.size
    in
    if emptyCt <= Dict.size bd then
        Just
            (if isEven (emptyCt - 1) then
                Cross

             else
                Zero
            )

    else
        Nothing


viewBD : BoardDict -> Svg Msg
viewBD bd =
    bd
        |> Dict.toList
        |> List.map viewCellEntry
        |> group []


viewCellEntry : ( GPos, Slot ) -> Svg Msg
viewCellEntry ( gp, marker ) =
    viewMarkerAt marker gp


viewMarkerAt : Slot -> GPos -> Svg Msg
viewMarkerAt marker gp =
    [ square 100 [ fill "blue", stroke "black" ]
    , words (slotToString marker) [ fill "white", xf [ scale 10 ] ]
    ]
        |> group
            (xf [ mvT <| gpToGridLocal { gridSize = 300, cellSize = 100 } gp ]
                :: notifyClick (OnClick gp)
                :: []
            )


slotToString : Slot -> String
slotToString slot =
    case slot of
        Marked Cross ->
            "X"

        Marked Zero ->
            "O"

        Empty ->
            ""
