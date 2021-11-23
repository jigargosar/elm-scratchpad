module SlideTileGame exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Grid exposing (Grid)
import Heap exposing (Heap)
import Html exposing (Attribute, Html, div, text)
import Html.Lazy
import PriorityQueue exposing (PriorityQueue)
import Random exposing (Generator)
import Search
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


iterationsPerFrame =
    3 * 1000


gw =
    3


gh =
    4


greedy =
    gw * gh > 9


cz =
    160


gpToWorld : GPos -> Vec
gpToWorld =
    vFromGP >> vScale cz >> vAdd1 (cz / 2)


type alias Model =
    { board : Board
    , search : List Search
    , aiSearch : Search.SearchResult Board
    , now : Int
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        board =
            solutionBoard
                |> always solutionBoard
                |> always initialBoard

        aiSearch =
            Search.aStar
                { step =
                    \b ->
                        allDir4
                            |> List.filterMap (\d -> slideTileInDirection d b)
                            |> List.map (\c -> ( c, c == solutionBoard ))
                , cost = always 1
                , heuristic = admissibleHeuristicCost >> toFloat
                }
                [ ( board, False ) ]
                |> Search.nextN 4000
                |> Debug.log "ans"
    in
    ( { board = board
      , search =
            [ initFrontierHP board |> startSolvingWithFrontier
            , initFrontierPQ board |> startSolvingWithFrontier

            --, initFrontierLS board |> startSolvingWithFrontier
            ]
      , aiSearch = aiSearch
      , now = 0
      }
    , Time.now |> Task.perform OnNow
    )


searchToSolutionAnimBoards : Search -> List Board
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
                | search = List.map (stepSearchN iterationsPerFrame) model.search

                --, aiSearch = Search.nextN 4000 model.aiSearch
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
            (model.search
                |> List.map
                    (\s ->
                        div [] [ viewSearch s, viewAnimBoards (searchToSolutionAnimBoards s) model.now ]
                    )
            )
        , viewBoard 0.6 model.board
        ]


viewAnimBoards : List Board -> Int -> Html Msg
viewAnimBoards boards time =
    nextListItemEvery 1 boards time
        |> Maybe.map (viewBoard 0.3)
        |> Maybe.withDefault (text "")


viewSearch : Search -> Html Msg
viewSearch search =
    case search of
        Found s n ->
            div []
                [ div []
                    [ if greedy && not (frontierEmpty s.frontier) then
                        text "Maybe Optimal"

                      else
                        text "Optimal"
                    ]
                , div [] [ text ("moves = " ++ String.fromInt n.pathToRootCost) ]
                , div [] [ text ("steps = " ++ String.fromInt s.steps) ]

                --, div []
                --    [ div [] [ text ("explored = " ++ String.fromInt (Dict.size s.explored)) ]
                --    , div [] [ text ("frontier = " ++ String.fromInt (frontierSize s.frontier)) ]
                --    ]
                , viewBoard 0.1 n.board
                ]

        Exhausted _ ->
            text "Fail: Search Space Exhausted"

        Searching s ->
            text <|
                "Searching... "
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
            , text (String.fromInt (admissibleHeuristicCost board))
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
        --(Random.initialSeed 10011)
        --(Random.initialSeed 5)
        --(Random.initialSeed 6)
        (Random.initialSeed 11)
        --(Random.initialSeed 0)
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
    moveTileAt (moveInDir4 (oppositeDir4 dir) board.e) board


moveTileAt : GPos -> Board -> Maybe Board
moveTileAt gp board =
    if areAdjacent board.e gp then
        board.g
            |> Grid.swap gp board.e
            |> Maybe.map (\g -> { e = gp, g = g })

    else
        Nothing


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
    , heuristicCost : Int
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


priorityOf : Node -> Int
priorityOf n =
    if greedy then
        n.heuristicCost

    else
        n.pathToRootCost + n.heuristicCost


isSolutionNode : Node -> Bool
isSolutionNode n =
    n.key == solutionBoardAsString


admissibleHeuristicCost : Board -> Int
admissibleHeuristicCost board =
    let
        admissibleCost ( currentGP, tile ) =
            if
                (currentGP == tileSolutionGP tile)
                    || (currentGP == board.e)
            then
                0

            else
                1
    in
    board.g
        |> Grid.toList
        |> sumBy admissibleCost


createChildrenNodes : Node -> List Node
createChildrenNodes p =
    let
        childFromBoard : Board -> Node
        childFromBoard board =
            { board = board
            , key = boardToKey board
            , heuristicCost = admissibleHeuristicCost board
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
    | HPFrontier FrontierHP


frontierEmpty : Frontier -> Bool
frontierEmpty frontier =
    case frontier of
        PQFrontier frontierPQ ->
            PriorityQueue.isEmpty frontierPQ

        HPFrontier frontierHP ->
            Heap.isEmpty frontierHP


frontierInsert : Frontier -> List Node -> Frontier
frontierInsert frontier =
    case frontier of
        PQFrontier pq ->
            List.foldl PriorityQueue.insert pq >> PQFrontier

        HPFrontier frontierHP ->
            List.foldl Heap.push frontierHP >> HPFrontier


initFrontierPQ : Board -> Frontier
initFrontierPQ board =
    PriorityQueue.empty priorityOf
        |> PriorityQueue.insert (rootNodeFromBoard board)
        |> PQFrontier


initFrontierHP : Board -> Frontier
initFrontierHP board =
    Heap.singleton (Heap.smallest |> Heap.by priorityOf)
        (rootNodeFromBoard board)
        |> HPFrontier


type alias State =
    { explored : Dict String Node
    , frontier : Frontier
    , steps : Int
    }


startSolvingWithFrontier : Frontier -> Search
startSolvingWithFrontier frontier =
    { explored = Dict.empty
    , frontier = frontier
    , steps = 0
    }
        |> Searching


rootNodeFromBoard : Board -> Node
rootNodeFromBoard board =
    { board = board
    , key = boardToKey board
    , heuristicCost = admissibleHeuristicCost board
    , pathToRootCost = 0
    , parent = None
    }


type Search
    = Searching State
    | Exhausted State
    | Found State Node


stepSearch : Search -> Search
stepSearch search =
    case search of
        Searching state ->
            stepSearchHelp state

        Found state g ->
            stepSearchBounded state g

        _ ->
            search


stepSearchBounded state g =
    case pop state.frontier of
        Nothing ->
            Found state g

        Just n ->
            Found state g


stepSearchHelp : State -> Search
stepSearchHelp state =
    case pop state.frontier of
        Nothing ->
            Exhausted state

        Just ( node, pendingFrontier ) ->
            if isSolutionNode node then
                Found
                    { explored = insertBy .key node state.explored
                    , frontier = pendingFrontier
                    , steps = state.steps + 1
                    }
                    node

            else
                let
                    explored =
                        Dict.update node.key
                            (\mbExplored ->
                                case mbExplored of
                                    Nothing ->
                                        Just node

                                    Just ex ->
                                        Just (minBy priorityOf ex node)
                            )
                            state.explored

                    rejectExploredChildren =
                        reject (\c -> Dict.member c.key explored)

                    rejectExpensiveExploredChildren =
                        reject
                            (\c ->
                                Dict.get c.key explored
                                    |> Maybe.map (\ex -> c.pathToRootCost >= ex.pathToRootCost)
                                    |> Maybe.withDefault False
                            )

                    filterChildren =
                        if False then
                            rejectExpensiveExploredChildren

                        else
                            rejectExploredChildren
                in
                Searching
                    { explored = explored
                    , frontier =
                        createChildrenNodes node
                            |> filterChildren
                            |> frontierInsert pendingFrontier
                    , steps = state.steps + 1
                    }


stepSearchN : Int -> Search -> Search
stepSearchN n =
    applyN n stepSearch


type alias FrontierPQ =
    PriorityQueue Node


type alias FrontierHP =
    Heap Node


type alias FrontierLS =
    List Node


pop : Frontier -> Maybe ( Node, Frontier )
pop frontier =
    case frontier of
        PQFrontier frontierPQ ->
            PriorityQueue.head frontierPQ
                |> Maybe.map (pairTo (PriorityQueue.tail frontierPQ |> PQFrontier))

        HPFrontier frontierHP ->
            Heap.pop frontierHP |> Maybe.map (mapSecond HPFrontier)


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
