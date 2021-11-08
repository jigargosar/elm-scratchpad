module SlideTileGame exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT
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
    480


height =
    width


gw =
    4


gh =
    4


cz =
    width / min gw gh


gpToWorld : GPos -> Vec
gpToWorld =
    vFromGP >> vScale cz >> vAdd1 (cz / 2)


type alias Model =
    { tiles : Dict GPos Tile
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tiles = initialTiles }
        |> onTileClick ( 2, 3 )
    , Cmd.none
    )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Svg.svg
        [ saWidth width
        , saHeight height
        , noFill
        , noStroke
        , bgc gray
        ]
        (model.tiles |> Dict.toList |> List.map viewTile)


type alias Tile =
    ( Int, GPos )


initialTiles : Dict GPos Tile
initialTiles =
    let
        gps =
            rangeWH gw gh
                |> List.take (gw * gh - 1)
    in
    gps
        |> List.indexedMap (\i gp -> ( gp, ( i, gp ) ))
        |> Dict.fromList


isEmptyGP : Model -> GPos -> Bool
isEmptyGP model gp =
    Debug.todo "todo"


onTileClick : GPos -> Model -> Model
onTileClick gp model =
    let
        adjGPS : List GPos
        adjGPS =
            adjacentGPS gw gh gp

        mbEmptyAdjGPS =
            List.filter (\k -> Dict.member k model.tiles |> not) adjGPS
                |> List.head

        updatedTiles =
            Maybe.map2
                (\emptyGP gpTile ->
                    model.tiles
                        |> Dict.remove gp
                        |> Dict.insert emptyGP gpTile
                )
                mbEmptyAdjGPS
                (Dict.get gp model.tiles)
                |> Maybe.withDefault model.tiles
    in
    { model | tiles = updatedTiles }


viewTile : ( GPos, Tile ) -> Html Msg
viewTile ( gp, ( i, _ ) ) =
    words
        [ fill white
        , xf [ mv (gpToWorld gp), scale 3 ]
        ]
        (String.fromInt (i + 1))
