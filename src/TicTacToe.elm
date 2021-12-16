module TicTacToe exposing (main)

import Browser
import Dict exposing (Dict)
import Utils exposing (..)


main =
    Browser.document
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
    = Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    Document "Tic Tac Toe - Game"
        [ basicStylesNode
        , viewBoardSvg model.bd
        ]


viewBoardSvg : BoardDict -> Html msg
viewBoardSvg bd =
    fRow [ h100, placeContentCenter ]
        [ svgBlock [ viewBoxC 300 300, ffMonospace ]
            [ viewBD bd
            ]
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


viewBD : BoardDict -> Svg msg
viewBD bd =
    bd
        |> Dict.toList
        |> List.map viewCellEntry
        |> group []


viewCellEntry : ( GPos, Marker ) -> Svg msg
viewCellEntry ( gp, marker ) =
    case marker of
        Cross ->
            viewSymbolAt "X" gp

        Zero ->
            viewSymbolAt "O" gp

        Empty ->
            viewSymbolAt "" gp


viewSymbolAt : String -> GPos -> Svg msg
viewSymbolAt str gp =
    [ square 100 [ fill "blue", stroke "black" ]
    , words str [ fill "white", xf [ scale 10 ] ]
    ]
        |> group (xf [ mvInSquareGrid { gridSize = 300, cellSize = 100 } gp ] :: [])


mvInSquareGrid : { a | gridSize : Float, cellSize : Float } -> GPos -> Transform
mvInSquareGrid { gridSize, cellSize } ( x, y ) =
    let
        c0 =
            -(gridSize / 2) + (cellSize / 2)
    in
    mvT ( c0 + toFloat x * cellSize, c0 + toFloat y * cellSize )
