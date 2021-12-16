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
                |> Dict.insert ( 0, 0 ) Cross
                |> Dict.insert ( 1, 1 ) Zero
                |> Dict.insert ( 2, 2 ) Cross
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
    let
        fn s =
            case s of
                Cross ->
                    Zero

                Zero ->
                    Empty

                Empty ->
                    Cross
    in
    { model | bd = Dict.update gp (Maybe.map fn) bd }


view : Model -> Document Msg
view model =
    Document "Tic Tac Toe - Game"
        [ basicStylesNode
        , viewBoardSvg model.bd
        ]


viewBoardSvg : BoardDict -> Html Msg
viewBoardSvg bd =
    svgBlock
        [ sMaxHeight "100vh"
        , viewBoxC 300 300
        , ffMonospace
        ]
        [ viewBD bd
        ]


type Marker
    = Cross
    | Zero
    | Empty


type alias BoardDict =
    Dict GPos Marker


emptyBoardDict : BoardDict
emptyBoardDict =
    squareGridPositions 3
        |> List.map (pairTo Empty)
        |> Dict.fromList


viewBD : BoardDict -> Svg Msg
viewBD bd =
    bd
        |> Dict.toList
        |> List.map viewCellEntry
        |> group []


viewCellEntry : ( GPos, Marker ) -> Svg Msg
viewCellEntry ( gp, marker ) =
    viewMarkerAt marker gp


viewMarkerAt : Marker -> GPos -> Svg Msg
viewMarkerAt marker gp =
    [ square 100 [ fill "blue", stroke "black" ]
    , words (markerToString marker) [ fill "white", xf [ scale 10 ] ]
    ]
        |> group
            (xf [ mvT <| gpToGridLocal { gridSize = 300, cellSize = 100 } gp ]
                :: notifyClick (OnClick gp)
                :: []
            )


markerToString : Marker -> String
markerToString marker =
    case marker of
        Cross ->
            "X"

        Zero ->
            "O"

        Empty ->
            ""
