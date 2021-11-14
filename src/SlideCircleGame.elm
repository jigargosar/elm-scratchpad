module SlideCircleGame exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div)
import Json.Decode as JD exposing (Decoder)
import Svg exposing (Svg, text)
import Svg.Attributes as SA
import Svg.Events as SE
import Time
import TypedSvg.Attributes as TA
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


gpToCenterWC : GPos -> Vec
gpToCenterWC =
    gpToLeftTopWC >> vAdd1 (cz / 2)


gpToLeftTopWC : GPos -> Vec
gpToLeftTopWC =
    vFromGP >> vScale cz


type alias Model =
    { tiles : Tiles
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tiles =
            initialTiles
                |> always solvedTiles
                |> always solvedTiles2
                |> always initialTiles
      }
    , Cmd.none
    )


type Msg
    = OnTick
    | GPClicked GPos
    | OnKeyDown String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 60) (\_ -> OnTick)
            |> always Sub.none
        , Browser.Events.onKeyDown (JD.map OnKeyDown keyDecoder)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )

        GPClicked gp ->
            ( { model | tiles = moveTileAt gp model.tiles }, Cmd.none )

        OnKeyDown key ->
            case arrowKeyToDir key of
                Just dir ->
                    ( { model | tiles = slideTileInDir dir model.tiles }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ pAll "10px", bgc gray ]
        [ Svg.svg
            [ saWidth width
            , saHeight height
            , noFill
            , noStroke
            , noUserSelect
            , overflowVisible
            ]
            [ viewTiles model.tiles
            , if isSolved model.tiles then
                group [ xf [ mvGridCenter ] ] <|
                    [ rect width
                        height
                        [ fill gray
                        , SA.opacity "0.9"
                        , xf []
                        ]
                    , words "GOOD JOB" [ fill white, xf [ scale 5 ] ]
                    ]

              else
                text ""
            ]
        ]


mvGridCenter =
    mv2 (width / 2) (height / 2)



-- TILE & TILES DICT


type alias Tile =
    { originalGP : GPos, key : String }


initTile : GPos -> Tile
initTile gp =
    { originalGP = gp, key = Debug.toString gp }


type alias TilesDict =
    Dict GPos Tile


smallCircleGP =
    ( 1, 2 )


initialEmptyGP =
    ( 1, 0 )


allGPS : List GPos
allGPS =
    rangeWH gw gh


solutionGPS : List GPos
solutionGPS =
    allGPS |> reject isFirstRow


type alias SolutionItem =
    { a : GPos, b : GPos, diff : GPos }


initSolutionItem : GPos -> GPos -> SolutionItem
initSolutionItem a b =
    { a = a
    , b = b
    , diff = map2 sub a b
    }


solutionItems : List SolutionItem
solutionItems =
    case solutionGPS of
        [] ->
            []

        h :: t ->
            List.map (initSolutionItem h) t


initialTilesDict : TilesDict
initialTilesDict =
    let
        insertAtOriginalGP t =
            Dict.insert t.originalGP t
    in
    allGPS
        |> List.foldl (initTile >> insertAtOriginalGP) Dict.empty
        |> Dict.remove initialEmptyGP


type alias Tiles =
    { empty : GPos
    , dict : TilesDict
    }


initialTiles : Tiles
initialTiles =
    { empty = smallCircleGP
    , dict = moveValueFromKeyToKey smallCircleGP initialEmptyGP initialTilesDict
    }


solvedTiles : Tiles
solvedTiles =
    { empty = initialEmptyGP
    , dict = initialTilesDict
    }


solvedTiles2 : Tiles
solvedTiles2 =
    { empty = ( 1, 3 )
    , dict =
        initialTilesDict
            |> renameKey (mapSecond (dec >> modBy 4))
    }


slideTileInDir : Dir4 -> Tiles -> Tiles
slideTileInDir dir tiles =
    moveTileAt (moveInDir4 dir tiles.empty) tiles


moveTileAt : GPos -> Tiles -> Tiles
moveTileAt gp tiles =
    case
        ( Dict.get gp tiles.dict
        , areAdjacent gp tiles.empty
        )
    of
        ( Just tile, True ) ->
            let
                updatedDict =
                    tiles.dict
                        |> Dict.remove gp
                        |> Dict.insert tiles.empty tile
            in
            { tiles | empty = gp, dict = updatedDict }

        _ ->
            tiles


isSolved : Tiles -> Bool
isSolved tiles =
    let
        originalToCurrentGPDict : Dict GPos GPos
        originalToCurrentGPDict =
            tiles.dict |> mapValueToSwapWithKey .originalGP

        getCurrentDiff : GPos -> GPos -> Maybe GPos
        getCurrentDiff oa ob =
            dictGet2 oa ob originalToCurrentGPDict
                |> Maybe.map (\( ca, cb ) -> map2 sub ca cb)

        originalDiffMatchesCurrentDiff : SolutionItem -> Bool
        originalDiffMatchesCurrentDiff { a, b, diff } =
            case getCurrentDiff a b of
                Nothing ->
                    False

                Just currentDiff ->
                    currentDiff == diff
    in
    List.all originalDiffMatchesCurrentDiff solutionItems


isFirstRow : GPos -> Bool
isFirstRow ( _, y ) =
    y == 0


viewTiles : Tiles -> Svg Msg
viewTiles tiles =
    tiles.dict
        |> Dict.toList
        |> List.sortBy (second >> .originalGP)
        |> List.map viewTileAt
        |> group []


viewTileAt : ( GPos, Tile ) -> Svg Msg
viewTileAt ( gp, t ) =
    group
        [ SE.onClick (GPClicked gp)
        , xf [ mv (gpToCenterWC gp), scale 0.97 ]
        , style "transition" "transform 200ms"
        ]
        [ square cz [ fill black, xf [ mv2 3 3 ] ]
        , square cz [ fill <| grayN 0.35 ]
        , viewTilePaint t
        ]


viewTilePaint : Tile -> Html msg
viewTilePaint t =
    let
        leftTop =
            gpToLeftTopWC t.originalGP
    in
    nestedSvg
        cz
        cz
        [ TA.viewBox leftTop.x leftTop.y cz cz ]
        [ viewGridBG ]


viewGridBG : Svg msg
viewGridBG =
    group
        [ SA.opacity "0.9"
        ]
        [ group [ xf [ mv (gpToCenterWC ( 1, 2 )) ] ]
            [ circle (cz * 0.15) [ stroke "black", SA.strokeWidth "50" ]
            , circle (cz * 0.15) [ stroke "aqua", SA.strokeWidth "15" ]
            ]
        , group [ xf [ mv (gpToCenterWC ( 1, 2 )) ] ]
            [ circle (cz * 1.1) [ stroke "black", SA.strokeWidth "60" ]
            , circle (cz * 1.1) [ stroke "aqua", SA.strokeWidth "25" ]
            ]
        , viewWAt ( 0, 0 )
        , viewWAt ( 2, 0 )
        ]


viewWAt : GPos -> Svg msg
viewWAt gp =
    group [ xf [ mv (gpToCenterWC gp) ] ]
        [ words "W" [ fill white, xf [ scale 7 ] ]
        ]
