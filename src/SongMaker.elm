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
    | OnPointerDown Int2


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnPointerDown gp ->
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
                |> List.map
                    (\y ->
                        let
                            gp =
                                ( x, y )
                        in
                        viewTile (colorAt gp) gp
                    )
                |> gCol []
    in
    rangeN w
        |> List.map viewColumnAtX
        |> gRow [ noUserSelect ]


viewTile c gp =
    div
        [ bgc c
        , sOutline "1px solid black"
        , sMinHeight "20px"
        , notifyPointerDown (OnPointerDown gp)
        ]
        []


sOutline =
    style "outline"
