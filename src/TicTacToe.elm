module TicTacToe exposing (main)

import Browser exposing (Document)
import Html
import Utils exposing (..)


main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


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
view _ =
    { title = "Tic Tac Toe - Game"
    , body =
        [ stylesNode
            """
                html,body{
                    height:100%;
                    background-color:#222;
                }
            """
        , viewBoard
        ]
    }


viewBoard =
    svg [ viewBoxC 300 300, dBlock, noFill, noStroke, ffMonospace ]
        [ viewCrossAt ( 0, 0 )
        , viewZeroAt ( 1, 1 )
        , viewCrossAt ( 2, 2 )
        ]


viewCrossAt : GPos -> Svg msg
viewCrossAt =
    viewSymbolAt "X"


viewZeroAt : GPos -> Svg msg
viewZeroAt =
    viewSymbolAt "O"


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
