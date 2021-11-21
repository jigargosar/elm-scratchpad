module SlideTileGame exposing (..)

import Array
import Browser
import Dict exposing (Dict)
import Grid exposing (Grid)
import Html exposing (Attribute, Html, div, text)
import PriorityQueue exposing (PriorityQueue)
import Random exposing (Generator)
import Svg exposing (Svg)
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


maxIterations =
    0.5 * 100 * 1000 |> round


iterationsPerFrame =
    3 * 1000


gw =
    3


gh =
    4


totalCellCount =
    gw * gh


cz =
    160


gpToWorld : GPos -> Vec
gpToWorld =
    vFromGP >> vScale cz >> vAdd1 (cz / 2)


type alias Model =
    { board : Board
    , loop : Loop State ( State, Maybe Node )
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        board =
            solutionBoard
                |> always initialBoard
    in
    ( { board = board, loop = solveBoard board }, Cmd.none )


type Msg
    = OnTick
    | GPClicked GPos
    | Nop


subscriptions : Model -> Sub Msg
subscriptions { loop } =
    case loop of
        Looping _ ->
            Time.every (1000 / 60) (\_ -> OnTick)

        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( { model | loop = stepLoopN iterationsPerFrame solveBoardHelp model.loop }
            , Cmd.none
            )

        GPClicked gp ->
            ( { model | board = moveTileAt gp model.board |> Maybe.withDefault model.board }, Cmd.none )

        Nop ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ fontSize "24px", dFlex, fDCol, gap "20px", pAll "20px" ]
        [ div [] [ viewLoop model.loop ]
        , viewBoardSvg model.board
        ]


viewLoop : Loop State ( State, Maybe Node ) -> Html Msg
viewLoop loop =
    case loop of
        Complete ( s, Just n ) ->
            div []
                [ div [] [ text ("moves = " ++ String.fromInt n.pathToRootCost) ]
                , div [] [ text ("steps = " ++ String.fromInt s.steps) ]

                --, viewScaledBoardSvg 0.3 n.board
                ]

        Complete ( _, Nothing ) ->
            text "Fail: Search Space Exhausted"

        Looping s ->
            text <|
                "Unable to find solution in "
                    ++ String.fromInt s.steps
                    ++ " steps. "
                    --++ String.fromInt (List.length s.frontier)
                    --++ " frontier length"
                    ++ ""


viewScaledBoardSvg : Float -> Board -> Html Msg
viewScaledBoardSvg scl board =
    Svg.svg
        [ saWidth (width * scl)
        , saHeight (height * scl)
        , TA.viewBox 0 0 width height
        , noFill
        , noStroke
        , bgc gray
        , noUserSelect
        ]
        [ viewBoard board
        ]


viewBoardSvg : Board -> Html Msg
viewBoardSvg =
    viewScaledBoardSvg 1


viewBoard : Board -> Svg Msg
viewBoard board =
    board.g
        |> Grid.toArray
        |> Array.toIndexedList
        |> List.map (mapFirst iToGP >> viewTile)
        |> group []


iToGP =
    Grid.indexToGP gw


type alias Tile =
    Int


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
        reduce i acc =
            String.fromInt i ++ "," ++ acc
    in
    board.g
        |> Grid.toArray
        |> Array.foldl reduce ""


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
            |> Maybe.map
                (\g ->
                    { board | e = gp, g = g }
                )

    else
        Nothing


moveTileAt : GPos -> Board -> Maybe Board
moveTileAt gp board =
    moveTileAt_A1 ( gp, board )


solutionBoard : Board
solutionBoard =
    { e = ( gw - 1, gh - 1 )
    , g = Grid.init gw gh (\( x, y ) -> y * gw + x)
    }


solutionBoardAsList : List Tile
solutionBoardAsList =
    solutionBoard.g
        |> Grid.toArray
        |> Array.toList


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


leastCostOf : Node -> Int
leastCostOf n =
    n.pathToRootCost + n.estimatedCostToReachSolution


isSolutionNode : Node -> Bool
isSolutionNode n =
    n.key == solutionBoardAsString


estimateCostToReachSolution : Board -> Int
estimateCostToReachSolution board =
    let
        reduce ( solutionGP, currentGP ) sum =
            sum + manhattenDistance solutionGP currentGP
    in
    board.g
        |> Grid.toArray
        |> Array.toIndexedList
        |> List.foldl (Tuple.mapBoth iToGP iToGP >> reduce) 0



--noinspection ElmUnusedSymbol


estimateCostToReachSolution1 : Board -> Int
estimateCostToReachSolution1 board =
    totalCellCount - solvedCellCount board


solvedCellCount : Board -> Int
solvedCellCount board =
    let
        reduce a ( ct, b ) =
            case b of
                h :: t ->
                    if h == a then
                        ( ct + 1, t )

                    else
                        ( ct, t )

                _ ->
                    ( ct, b )
    in
    board.g
        |> Grid.toArray
        |> Array.foldl reduce ( 0, solutionBoardAsList )
        |> first


allDirections =
    [ Up, Down, Left, Right ]


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
    allDirections
        |> List.filterMap
            (slideParentBoardInDir >> Maybe.map childFromBoard)


type alias State =
    { explored : Dict String Node
    , frontier : Frontier
    , steps : Int
    }


initState : Board -> State
initState b =
    let
        rootNode =
            { board = b
            , key = boardToKey b
            , estimatedCostToReachSolution = estimateCostToReachSolution b
            , pathToRootCost = 0
            , parent = None
            }
    in
    { explored = Dict.empty
    , frontier = PriorityQueue.empty leastCostOf |> PriorityQueue.insert rootNode
    , steps = 0
    }


type LoopResult state result
    = Loop state
    | Done result


type Loop state result
    = Looping state
    | Complete result


stepLoop : (state -> LoopResult state result) -> Loop state result -> Loop state result
stepLoop fn loop =
    case loop of
        Looping state ->
            case fn state of
                Loop newState ->
                    Looping newState

                Done result ->
                    Complete result

        Complete _ ->
            loop


isComplete : Loop state result -> Bool
isComplete loop =
    case loop of
        Complete _ ->
            True

        _ ->
            False


stepLoopN : Int -> (state -> LoopResult state result) -> Loop state result -> Loop state result
stepLoopN n fn loop =
    if n <= 0 || isComplete loop then
        loop

    else
        stepLoopN (n - 1) fn (stepLoop fn loop)


solveBoard : Board -> Loop State ( State, Maybe Node )
solveBoard board =
    initState board
        |> Looping
        |> stepLoopN maxIterations solveBoardHelp


type alias Frontier =
    PriorityQueue Node


pop : Frontier -> Maybe ( Node, Frontier )
pop frontier =
    PriorityQueue.head frontier
        |> Maybe.map (pairTo (PriorityQueue.tail frontier))


isExplored : State -> Node -> Bool
isExplored state node =
    isExplored_A1 ( state, node )


isExplored_A1 : ( State, Node ) -> Bool
isExplored_A1 ( state, node ) =
    Dict.member node.key state.explored


solveBoardHelp : State -> LoopResult State ( State, Maybe Node )
solveBoardHelp state =
    case pop state.frontier of
        Nothing ->
            Done ( state, Nothing )

        Just ( node, pendingFrontier ) ->
            if isSolutionNode node then
                Done ( state, Just node )

            else
                { explored = Dict.insert node.key node state.explored
                , frontier =
                    createChildrenNodes node
                        |> reject (isExplored state)
                        |> List.foldl PriorityQueue.insert pendingFrontier
                , steps = state.steps + 1
                }
                    |> Loop


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
