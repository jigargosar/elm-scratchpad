module SlideTileGame exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Grid exposing (Grid)
import Html exposing (Attribute, Html, div, text)
import Html.Lazy
import PriorityQueue exposing (PriorityQueue)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Events as SE
import Task
import Time exposing (Posix)
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


maxIterations =
    50 * 1000 |> round


iterationsPerFrame =
    3 * 1000


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
    , search : Search State Node
    , search2 : Search State Node
    , now : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        board =
            solutionBoard
                |> always solutionBoard
                |> always initialBoard
    in
    ( { board = board
      , search = initFrontierPQ board |> startSolvingWithFrontier
      , search2 = initFrontierLS board |> startSolvingWithFrontier
      , now = 0
      }
    , Time.now |> Task.perform OnNow
    )


searchToSolutionAnimBoards : Search state Node -> List Board
searchToSolutionAnimBoards search =
    case search of
        Found _ n ->
            nodeAncestorBoards n []
                |> (\( h, t ) -> h :: t)

        _ ->
            []


type Msg
    = OnTick
    | GPClicked GPos
    | Nop
    | OnNow Posix


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1000 / 60) (\_ -> OnTick)
        , Browser.Events.onAnimationFrame OnNow
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( { model
                | search = stepSearchN iterationsPerFrame model.search
                , search2 = stepSearchN iterationsPerFrame model.search2
              }
            , Cmd.none
            )

        GPClicked gp ->
            ( { model | board = moveTileAt gp model.board |> Maybe.withDefault model.board }, Cmd.none )

        Nop ->
            ( model, Cmd.none )

        OnNow posix ->
            ( { model | now = Time.posixToMillis posix }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ fontSize "24px", dFlex, fDCol, gap "20px", pAll "20px" ]
        [ div [ dFlex, gap "20px" ]
            [ viewSearch model.search
            , viewSearch model.search2
            , viewAnimBoards (searchToSolutionAnimBoards model.search) model.now
            ]
        , viewBoard 0.6 model.board
        ]


viewAnimBoards : List Board -> Int -> Html Msg
viewAnimBoards boards time =
    nextListItemEvery 1 boards time
        |> Maybe.map (viewBoard 0.3)
        |> Maybe.withDefault (text "")


viewSearch : Search State Node -> Html Msg
viewSearch search =
    case search of
        Found s n ->
            div []
                [ div [] [ text ("moves = " ++ String.fromInt n.pathToRootCost) ]
                , div [] [ text ("steps = " ++ String.fromInt s.steps) ]
                , viewBoard 0.3 n.board
                ]

        Exhausted _ ->
            text "Fail: Search Space Exhausted"

        Searching s ->
            text <|
                "Unable to find solution in "
                    ++ String.fromInt s.steps
                    ++ " steps. "
                    --++ String.fromInt (List.length s.frontier)
                    --++ " frontier length"
                    ++ ""


viewBoard : Float -> Board -> Html Msg
viewBoard scl board =
    div []
        [ div []
            [ text "heuristic="
            , text (String.fromInt (estimateCostToReachSolution board))
            ]
        , Svg.svg
            [ saWidth (width * scl)
            , saHeight (height * scl)
            , TA.viewBox 0 0 width height
            , noFill
            , noStroke
            , bgc gray
            , noUserSelect
            ]
            [ Html.Lazy.lazy viewBoardHelp board
            ]
        ]


viewBoardHelp : Board -> Svg Msg
viewBoardHelp board =
    board.g
        |> Grid.toList
        |> reject (first >> eq board.e)
        |> List.sortBy (second >> tileIndex)
        |> List.map viewTile
        |> group []


type alias Tile =
    ( Int, GPos )


tileIndex : Tile -> Int
tileIndex =
    first


tileSolutionGP : Tile -> GPos
tileSolutionGP =
    second


tileViewIndex : Tile -> String
tileViewIndex =
    tileIndex >> inc >> String.fromInt


tileFromIndexAndSolutionGP : Int -> GPos -> Tile
tileFromIndexAndSolutionGP =
    Tuple.pair


type alias Board =
    { e : GPos, g : Grid Tile }


initialBoard : Board
initialBoard =
    Random.step randomBoard
        (Random.initialSeed 0)
        |> first


boardToKey : Board -> String
boardToKey board =
    let
        reduce tile acc =
            String.fromInt (tileIndex tile) ++ "," ++ acc
    in
    board.g |> Grid.foldValues reduce ""


randomBoard : Generator Board
randomBoard =
    let
        slideInDirections : List Dir4 -> Board
        slideInDirections =
            List.foldl (\dir -> withRollback (slideTileInDirection dir)) solutionBoard
    in
    Random.list 100 randomDir
        |> Random.map slideInDirections


slideTileInDirection : Dir4 -> Board -> Maybe Board
slideTileInDirection dir board =
    slideTileInDirection_A1 ( dir, board )


slideTileInDirection_A1 : ( Dir4, Board ) -> Maybe Board
slideTileInDirection_A1 ( dir, board ) =
    moveTileAt (moveInDir4 (oppositeDir4 dir) board.e) board


moveTileAt_A1 : ( GPos, Board ) -> Maybe Board
moveTileAt_A1 ( gp, board ) =
    if areAdjacent board.e gp then
        board.g
            |> Grid.swap gp board.e
            |> Maybe.map (\g -> { e = gp, g = g })

    else
        Nothing


moveTileAt : GPos -> Board -> Maybe Board
moveTileAt gp board =
    moveTileAt_A1 ( gp, board )


solutionBoard : Board
solutionBoard =
    { e = ( gw - 1, gh - 1 )
    , g = Grid.initIndexed gw gh tileFromIndexAndSolutionGP
    }


solutionBoardAsString =
    boardToKey solutionBoard


type alias Node =
    { board : Board
    , key : String
    , estimatedCostToReachSolution : Int
    , pathToRootCost : Int
    , parent : Parent
    }


type Parent
    = None
    | Parent Node


nodeAncestorBoards : Node -> List Board -> ( Board, List Board )
nodeAncestorBoards n acc =
    case n.parent of
        None ->
            ( n.board, acc )

        Parent p ->
            nodeAncestorBoards p (n.board :: acc)


leastCostOf : Node -> Int
leastCostOf n =
    n.pathToRootCost + n.estimatedCostToReachSolution


isSolutionNode : Node -> Bool
isSolutionNode n =
    n.key == solutionBoardAsString


estimateCostToReachSolution : Board -> Int
estimateCostToReachSolution board =
    let
        tileManhattanCostToSolution ( currentGP, tile ) =
            manhattenDistance currentGP (tileSolutionGP tile)
    in
    board.g
        |> Grid.toList
        |> sumBy tileManhattanCostToSolution


createChildrenNodes : Node -> List Node
createChildrenNodes p =
    let
        childFromBoard : Board -> Node
        childFromBoard board =
            { board = board
            , key = boardToKey board
            , estimatedCostToReachSolution = estimateCostToReachSolution board
            , pathToRootCost = p.pathToRootCost + 1
            , parent = Parent p
            }

        slideParentBoardInDir dir =
            slideTileInDirection dir p.board
    in
    allDir4
        |> List.filterMap
            (slideParentBoardInDir >> Maybe.map childFromBoard)


type Frontier
    = PQFrontier FrontierPQ
    | LSFrontier FrontierLS


frontierInsert : Frontier -> List Node -> Frontier
frontierInsert frontier =
    case frontier of
        PQFrontier pq ->
            List.foldl PriorityQueue.insert pq >> PQFrontier

        LSFrontier ls ->
            (++) ls >> LSFrontier


initFrontierPQ : Board -> Frontier
initFrontierPQ board =
    PriorityQueue.empty leastCostOf
        |> PriorityQueue.insert (rootNodeFromBoard board)
        |> PQFrontier


initFrontierLS : Board -> Frontier
initFrontierLS board =
    LSFrontier [ rootNodeFromBoard board ]


type alias State =
    { explored : Dict String Node
    , frontier : Frontier
    , steps : Int
    }


startSolvingWithFrontier : Frontier -> Search State Node
startSolvingWithFrontier frontier =
    { explored = Dict.empty
    , frontier = frontier
    , steps = 0
    }
        |> Searching
        |> stepSearchN maxIterations


rootNodeFromBoard : Board -> Node
rootNodeFromBoard board =
    { board = board
    , key = boardToKey board
    , estimatedCostToReachSolution = estimateCostToReachSolution board
    , pathToRootCost = 0
    , parent = None
    }


type Search state answer
    = Searching state
    | Exhausted state
    | Found state answer


stepSearch : Search State Node -> Search State Node
stepSearch search =
    case search of
        Searching state ->
            stepSearchHelp state

        _ ->
            search


stepSearchN : Int -> Search State Node -> Search State Node
stepSearchN n =
    applyN n stepSearch


type alias FrontierPQ =
    PriorityQueue Node


type alias FrontierLS =
    List Node


pop : Frontier -> Maybe ( Node, Frontier )
pop frontier =
    case frontier of
        PQFrontier frontierPQ ->
            PriorityQueue.head frontierPQ
                |> Maybe.map (pairTo (PriorityQueue.tail frontierPQ |> PQFrontier))

        LSFrontier frontierLS ->
            popFrontierLS frontierLS
                |> Maybe.map (mapSecond LSFrontier)


popFrontierLS : FrontierLS -> Maybe ( Node, FrontierLS )
popFrontierLS frontierLS =
    let
        reduce n ( min, acc ) =
            if leastCostOf n <= leastCostOf min then
                ( n, min :: acc )

            else
                ( min, n :: acc )

        pop2Help ( h, t ) =
            List.foldl reduce ( h, [] ) t
    in
    uncons frontierLS
        |> Maybe.map pop2Help


isExplored state node =
    isExplored_A1 ( state, node )


isExplored_A1 ( state, node ) =
    Dict.member node.key state.explored


stepSearchHelp : State -> Search State Node
stepSearchHelp state =
    case pop state.frontier of
        Nothing ->
            Exhausted state

        Just ( node, pendingFrontier ) ->
            if isSolutionNode node then
                Found state node

            else
                Searching
                    { explored = insertBy .key node state.explored
                    , frontier =
                        createChildrenNodes node
                            |> reject (isExplored state)
                            |> frontierInsert pendingFrontier
                    , steps = state.steps + 1
                    }


viewTile : ( GPos, Tile ) -> Html Msg
viewTile ( gp, tile ) =
    group [ SE.onClick (GPClicked gp) ]
        [ group [ transitionTransform, xf [ mv (gpToWorld gp) ] ]
            [ square cz [ fillTransparent ]
            , words
                (tileViewIndex tile)
                [ fill white
                , xf [ scale 3 ]
                ]
            ]
        ]
