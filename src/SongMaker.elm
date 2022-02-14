port module SongMaker exposing (main)

import Dict exposing (Dict)
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


port playSingleNote : String -> Cmd msg


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
      , pp = paintedPositions |> always Set.empty
      , cIdx = 0
      }
    , Cmd.none
    )


toNotesColumns : Int -> Set Int2 -> List (List String)
toNotesColumns w pp =
    let
        _ =
            [ [ "C4" ]
            , [ "E4", "D4", "E4" ]
            , [ "G4" ]
            , [ "A4", "G4" ]
            ]
                |> List.concat
                |> List.map List.singleton

        columnToNotesDict : Dict Int (List String)
        columnToNotesDict =
            groupEqBy first (Set.toList pp)
                |> List.map (\( gp, gps ) -> ( first gp, List.map noteFromGP (gp :: gps) ))
                |> Dict.fromList
    in
    rangeN w
        |> List.map (\x -> Dict.get x columnToNotesDict |> Maybe.withDefault [])


noteFromGP : Int2 -> String
noteFromGP ( _, y ) =
    listGetAtWithDefault
        ""
        y
        [ "C4"
        , "D4"
        , "E4"
        , "F4"
        , "G4"
        , "A4"
        , "B4"
        , "C5"
        , "D5"
        , "E5"
        , "F5"
        , "G5"
        , "A5"
        , "B5"
        ]


type Msg
    = NOP
    | OnPointerDown Int2
    | PlayClicked
    | StopClicked
    | SelectColumn Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    selectColumn SelectColumn


playEffect : Model -> Cmd msg
playEffect model =
    play (toNotesColumns model.w model.pp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnPointerDown gp ->
            (if Set.member gp model.pp then
                { model | pp = Set.remove gp model.pp }
                    |> withNoCmd

             else
                { model | pp = Set.insert gp model.pp }
                    |> withCmd (playSingleNote (noteFromGP gp))
            )
                |> addEffect playEffect

        PlayClicked ->
            model |> withEffect playEffect

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
