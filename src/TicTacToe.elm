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
                |> withRollback (makeMove ( 0, 1 ))
                |> withRollback (makeMove ( 1, 1 ))
                |> withRollback (makeMove ( 0, 2 ))
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
        , getNextMark bd
        )
    of
        ( Nothing, Just Empty, Just mark ) ->
            Just (Dict.insert gp (Marked mark) bd)

        _ ->
            Nothing


getWinner : BoardDict -> Maybe Mark
getWinner bd =
    let
        columns =
            times 3 (\x -> times 3 (\y -> ( x, y )))

        rows =
            times 3 (\y -> times 3 (\x -> ( x, y )))

        diagonal1 =
            times 3 (\n -> ( n, n ))

        diagonal2 =
            times 3 (\n -> ( n, 2 - n ))
    in
    [ diagonal1, diagonal2 ]
        ++ columns
        ++ rows
        |> List.filterMap (winnerFromGPS bd)
        |> List.head


winnerFromGPS : BoardDict -> List GPos -> Maybe Mark
winnerFromGPS bd gps =
    case List.filterMap (getInDict bd) gps of
        ((Marked mark) as h) :: t ->
            maybeFromBool (allEq h t) mark

        _ ->
            Nothing


getNextMark : BoardDict -> Maybe Mark
getNextMark bd =
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
