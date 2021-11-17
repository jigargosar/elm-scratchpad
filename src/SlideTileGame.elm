module SlideTileGame exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Svg exposing (Svg)
import Svg.Events as SE
import Time
import Utils exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


width =
    gw * cz


height =
    gh * cz


gw =
    3


gh =
    4


cz =
    160


gpToWorld : GPos -> Vec
gpToWorld =
    vFromGP >> vScale cz >> vAdd1 (cz / 2)


type alias Model =
    { tiles : TilesDict
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tiles = initialTiles }
    , Cmd.none
    )


type Msg
    = OnTick
    | GPClicked GPos


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )

        GPClicked gp ->
            ( onGPClick gp model, Cmd.none )


view : Model -> Html Msg
view model =
    Svg.svg
        [ saWidth width
        , saHeight height
        , noFill
        , noStroke
        , bgc gray
        , noUserSelect
        ]
        [ model.tiles
            |> Dict.toList
            |> List.map viewTile3
            |> group []
        ]


type alias Tile =
    ( Int, GPos )


type alias TilesDict =
    Dict GPos Tile


initialTiles : TilesDict
initialTiles =
    let
        gps =
            rangeWH gw gh

        --|> List.take (gw * gh - 1)
    in
    gps
        |> List.indexedMap (\i gp -> ( gp, ( i, gp ) ))
        |> List.filter (\( _, ( i, _ ) ) -> i /= (8 - 1))
        |> Dict.fromList


getEmptyGP : TilesDict -> GPos
getEmptyGP td =
    case
        rangeWH gw gh
            |> List.filter (\k -> Dict.member k td |> not)
            |> List.head
    of
        Nothing ->
            Debug.todo "this should never happen"

        Just gp ->
            gp


onGPClick : GPos -> Model -> Model
onGPClick gp model =
    let
        updatedTiles =
            case ( Dict.get gp model.tiles, areAdjacent gp (getEmptyGP model.tiles) ) of
                ( Just gpTile, True ) ->
                    model.tiles
                        |> Dict.remove gp
                        |> Dict.insert (getEmptyGP model.tiles) gpTile

                _ ->
                    model.tiles
    in
    { model | tiles = updatedTiles }


viewTile : ( GPos, Tile ) -> Html Msg
viewTile ( gp, ( i, _ ) ) =
    group [ xf [ mv (gpToWorld gp) ] ]
        [ square cz [ fillTransparent ]
        , words
            (String.fromInt (i + 1))
            [ fill white
            , xf [ scale 3 ]
            ]
        ]


viewTile3 (( gp, _ ) as x) =
    group [ SE.onClick (GPClicked gp) ]
        [ viewTile x
        ]
