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


port play : () -> Cmd msg


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    { w : Int, h : Int, pp : Set Int2 }


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
    ( { w = w, h = h, pp = paintedPositions }, Cmd.none )


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
            ( { model | pp = setToggleMember gp model.pp }, Cmd.none )


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
        , fRow [ pa "20px" ] [ button [ fontSize "20px", pa "0.3em 1em" ] [ text "Play" ] ]
        ]


viewGrid : Model -> Html Msg
viewGrid { w, h, pp } =
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
                |> gCol []
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
