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
        colorAt : ( a, Int ) -> String
        colorAt ( _, y ) =
            listGetAtWithDefault "" (modBy 7 y) colors

        w =
            16

        h =
            14
    in
    rangeN w
        |> List.map (\x -> rangeN h |> List.map (\y -> colorAt ( x, y )) |> viewColumn)
        |> gRow []


viewColumn =
    List.map viewTile >> gCol []


viewTile c =
    div [ bgc c, sOutline "1px solid black", sMinHeight "20px" ] []


sOutline =
    style "outline"
