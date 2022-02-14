port module SongMaker exposing (main)

import Html exposing (button)
import Random
import Random.List
import Set exposing (Set)
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


port play : List (List String) -> Cmd msg


port stop : () -> Cmd msg


port selectColumn : (Int -> msg) -> Sub msg


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    { w : Int, h : Int, pp : Set Int2, cIdx : Int }


init : () -> ( Model, Cmd Msg )
init () =
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
    in
    ( { w = w
      , h = h
      , pp = paintedPositions
      , cIdx = 0
      }
    , Cmd.none
    )


toNotesColumns : Set Int2 -> List (List String)
toNotesColumns _ =
    [ [ "C4" ]
    , [ "E4", "D4", "E4" ]
    , [ "G4" ]
    , [ "A4", "G4" ]
    ]
        |> List.concat
        |> List.map List.singleton


type Msg
    = NOP
    | OnPointerDown Int2
    | PlayClicked
    | StopClicked
    | SelectColumn Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    selectColumn SelectColumn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnPointerDown gp ->
            ( { model | pp = setToggleMember gp model.pp }, Cmd.none )

        PlayClicked ->
            ( model, play (toNotesColumns model.pp) )

        StopClicked ->
            ( model, stop () )

        SelectColumn cIdx ->
            ( { model | cIdx = cIdx }, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument model =
    Document "Song Maker"
        [ basicStylesNode
        , view model
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


view : Model -> Html Msg
view ({ w, h, pp } as model) =
    fCol []
        [ viewGrid model
        , fRow [ pa "20px", gap "20px" ]
            [ button
                [ fontSize "20px"
                , pa "0.3em 1em"
                , notifyClick PlayClicked
                ]
                [ text "Play" ]
            , button
                [ fontSize "20px"
                , pa "0.3em 1em"
                , notifyClick StopClicked
                ]
                [ text "Stop" ]
            ]
        ]


viewGrid : Model -> Html Msg
viewGrid { w, h, pp, cIdx } =
    let
        colorAt : Int2 -> String
        colorAt (( _, y ) as gp) =
            if Set.member gp pp then
                listGetAtWithDefault "" (modBy 7 y) colors

            else
                "transparent"

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
                |> gCol
                    [ opacity
                        (if x == cIdx then
                            0.5

                         else
                            1
                        )
                    ]
    in
    rangeN w
        |> List.map viewColumnAtX
        |> gRow [ style "flex-grow" "1", noUserSelect ]


viewTile c gp =
    div
        [ bgc c
        , sOutline ("0.5px solid " ++ wLightGray)
        , sMinHeight "20px"
        , notifyPointerDown (OnPointerDown gp)
        ]
        []
