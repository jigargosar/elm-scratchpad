module SlideCircleGame exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Time
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as Px
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
            |> List.map viewTileAt
            |> group []
        ]


viewTileAt : ( GPos, Tile ) -> Svg Msg
viewTileAt ( gp, t ) =
    group
        [ SE.onClick (GPClicked gp)
        , xf [ mv (gpToWorld gp) ]
        ]
        [ viewTileFG t
        , viewTileBG t
        ]


viewCircles : Svg msg
viewCircles =
    group
        [ stroke "aqua"
        , SA.strokeWidth "20"
        , SA.opacity "0.7"
        , xf [ mv2 (width / 2) (height / 2) ]
        ]
        [ circle (cz * 1.3) [ xf [ mv2 0 (cz / 2) ] ]
        , circle (cz * 0.3) [ xf [ mv2 0 (-height * 0.5 + cz * 0.5) ] ]
        ]


type alias Tile =
    { originalGP : GPos }


type alias TilesDict =
    Dict GPos Tile


initialTiles : TilesDict
initialTiles =
    let
        allGPs =
            rangeWH gw gh

        insertTile t =
            Dict.insert t.originalGP t
    in
    allGPs
        |> List.filter (gpToTileViewIndex >> neq 8)
        |> List.map Tile
        |> List.foldl insertTile Dict.empty


gpToTileViewIndex : GPos -> Int
gpToTileViewIndex ( x, y ) =
    y * gw + x + 1


getTileViewIndex : Tile -> Int
getTileViewIndex tile =
    gpToTileViewIndex tile.originalGP


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


viewIndex : Tile -> String
viewIndex tile =
    getTileViewIndex tile |> String.fromInt


viewTileBG : Tile -> Html msg
viewTileBG t =
    let
        ( ogx, ogy ) =
            t.originalGP
    in
    Svg.svg
        [ saWidth cz
        , saHeight cz
        , TA.viewBox (toFloat ogx * cz) (toFloat ogy * cz) cz cz
        , Px.x <| (cz / -2)
        , Px.y <| (cz / -2)
        ]
        [ viewCircles ]


viewTileFG : Tile -> Svg msg
viewTileFG t =
    group []
        [ square cz [ fillTransparent ]
        , words
            [ fill white
            , xf [ scale 3 ]
            ]
            (viewIndex t)
        ]
