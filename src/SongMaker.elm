module SongMaker exposing (main)

import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


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
    Document "Song Maker"
        [ basicStylesNode
        , view
        ]


colors =
    [ wPurple
    , wGreen_lime
    , wYellow
    , wOrange
    , wPink
    , wBlue
    , wGreen2_sea
    ]


view =
    div [ dGrid ]
        (colors |> List.map viewTile)


viewTile c =
    div [ bgc c, sOutline "1px solid black", sMinHeight "20px" ] []


sOutline =
    style "outline"
