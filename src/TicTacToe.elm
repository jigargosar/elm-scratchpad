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
        [ --words "X" [ fill "white", xf [ scale 10 ] ]
          viewCross
        ]


viewCross =
    [ square 100 [ fill "blue", stroke "black" ]
    , words "O" [ fill "white", xf [ scale 10 ] ]
    ]
        |> group []
