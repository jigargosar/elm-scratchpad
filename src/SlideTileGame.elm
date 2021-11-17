module SlideTileGame exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Random exposing (Generator)
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
    { board : Board
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { board =
            solutionBoard
                |> always initialBoard
      }
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
            ( { model | board = moveTileAt gp model.board }, Cmd.none )


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
        [ model.board.d
            |> Dict.toList
            |> List.map viewTile
            |> group []
        ]


type alias Tile =
    Int


type alias Board =
    { e : GPos, d : Dict GPos Tile }


initialBoard : Board
initialBoard =
    Random.step randomBoard
        (Random.initialSeed 0)
        |> first


randomBoard : Generator Board
randomBoard =
    let
        slideInDir4 : Dir4 -> Board -> Board
        slideInDir4 dir board =
            moveTileAt (moveInDir4 dir board.e) board

        slideInDirections : List Dir4 -> Board
        slideInDirections =
            List.foldl slideInDir4 solutionBoard
    in
    Random.list 100 randomDir
        |> Random.map slideInDirections


solutionBoard : Board
solutionBoard =
    let
        gps =
            rangeWH gw gh
                |> List.take (gw * gh - 1)

        d =
            gps
                |> List.indexedMap (\i gp -> ( gp, i ))
                |> Dict.fromList
    in
    { e = ( gw - 1, gh - 1 ), d = d }


type PriorityQueue
    = PriorityQueue


type Node
    = Node


isSolutionNode : Node -> Bool
isSolutionNode =
    Debug.todo "todo"


createChildrenNodes : Node -> List Node
createChildrenNodes =
    Debug.todo "todo"


enqueueAll : List Node -> PriorityQueue -> PriorityQueue
enqueueAll =
    Debug.todo "todo"


dequeue : PriorityQueue -> Maybe ( Node, PriorityQueue )
dequeue =
    Debug.todo "todo"


solvePriorityQueue : PriorityQueue -> Maybe Node
solvePriorityQueue pq =
    case dequeue pq of
        Nothing ->
            Nothing

        Just ( node, pendingPQ ) ->
            if isSolutionNode node then
                Just node

            else
                solvePriorityQueue (enqueueAll (createChildrenNodes node) pendingPQ)



--solveHelp: Node -> Acc -> List Dir4
--solveHelp node acc =
--    if node.board == solutionBoard then
--        node.path
--    else
--


moveTileAt : GPos -> Board -> Board
moveTileAt gp board =
    case ( Dict.get gp board.d, areAdjacent board.e gp ) of
        ( Just gpTile, True ) ->
            { board
                | e = gp
                , d =
                    board.d
                        |> Dict.remove gp
                        |> Dict.insert board.e gpTile
            }

        _ ->
            board


viewTile : ( GPos, Tile ) -> Html Msg
viewTile ( gp, i ) =
    group [ SE.onClick (GPClicked gp) ]
        [ group [ xf [ mv (gpToWorld gp) ] ]
            [ square cz [ fillTransparent ]
            , words
                (String.fromInt (i + 1))
                [ fill white
                , xf [ scale 3 ]
                ]
            ]
        ]
