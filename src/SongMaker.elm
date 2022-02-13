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
    [ wPink
    , wPurple
    , wBlue
    , wGreen2_sea
    , wOrange
    , wYellow
    , wGreen_lime
    ]


view =
    let
        columnTiles =
            List.repeat 2 colors |> List.concat

        columnList =
            List.repeat 16 columnTiles
    in
    columnList
        |> viewColumn
        |> gRow []


viewColumn : List (List String) -> List (Html msg)
viewColumn =
    List.map
        (\cs ->
            cs
                |> List.map viewTile
                |> gCol []
        )


viewTile c =
    div [ bgc c, sOutline "1px solid black", sMinHeight "20px" ] []


sOutline =
    style "outline"
