module SongMaker exposing (main)

import Random
import Random.List
import Set
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
        w =
            16

        h =
            14
    in
    let
        paintedPositions =
            rangeWH w h
                |> Random.List.shuffle
                |> Random.andThen Random.List.shuffle
                |> stepWithInitialSeed 0
                |> List.take 40
                |> Set.fromList

        colorAt : Int2 -> String
        colorAt (( _, y ) as gp) =
            if Set.member gp paintedPositions then
                listGetAtWithDefault "" (modBy 7 y) colors

            else
                "white"

        viewColumnAtX x =
            rangeN h
                |> List.map (\y -> colorAt ( x, y ))
                |> List.map viewTile
                |> gCol []
    in
    rangeN w
        |> List.map viewColumnAtX
        |> gRow []


viewTile c =
    div [ bgc c, sOutline "1px solid black", sMinHeight "20px" ] []


sOutline =
    style "outline"
