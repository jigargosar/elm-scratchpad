module PongCS50 exposing (main)

import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:
    * Clone: https://www.youtube.com/watch?v=GfwpRU0cT10&list=PLhQjrBD2T383Vx9-4vJYFsJbvZ_D17Qzh&index=2
    # Why?
    * create a video/text tutorial series.
    * code kata
    * make a complete game

    # Next up
    * rectangle update

    # Later
    * retro font update




-}


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    { leftPaddleCY : Float
    , rightPaddleCY : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { leftPaddleCY = 0, rightPaddleCY = 0 }, Cmd.none )


type Msg
    = NOP


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument _ =
    Document "PongCS50"
        [ basicStylesNode
        , view
        ]


sceneWidth =
    300


sceneHeight =
    sceneWidth / sceneAspectRatio


sceneLeft =
    -(sceneWidth / 2)


sceneRight =
    -sceneLeft


sceneTop =
    -(sceneHeight / 2)


sceneBottom =
    -sceneTop


sceneAspectRatio =
    16 / 9


ballSide =
    sceneWidth / 50


paddleWidth =
    ballSide


paddleHeight =
    sceneHeight / 6


paddleMargin =
    ballSide * 2


paddleXOffset =
    paddleMargin + (paddleWidth / 2)


leftPaddleX =
    sceneLeft + paddleXOffset


rightPaddleX =
    sceneRight - paddleXOffset


view : Html Msg
view =
    basicSvg [ viewBoxC sceneWidth sceneHeight, sMaxHeight "100vh" ]
        [ square ballSide [ fill wWhite, transforms [] ]
        , viewPaddle leftPaddleX
        , viewPaddle rightPaddleX
        ]


viewPaddle x =
    rect paddleWidth paddleHeight <|
        [ fill wWhite, transforms [ translateF2 ( x, 0 ) ] ]
