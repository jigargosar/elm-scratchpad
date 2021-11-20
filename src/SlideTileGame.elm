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
    , loop : Loop State (Maybe Node)
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
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( model, Cmd.none )

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


viewLoop : Loop State (Maybe Node) -> Html Msg
viewLoop loop =
    case loop of
        Complete (Just n) ->
            div []
                [ div [] [ text ("moves = " ++ String.fromInt n.pathToRootCost) ]
                , viewScaledBoardSvg 0.3 n.board
                ]

        Complete Nothing ->
            text "Fail: Search Space Exhausted"

        Looping _ ->
            text <|
                "Unable to find solution in "
                    ++ String.fromInt maxIterations
                    ++ " iterations. "
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


iToGP i =
    ( modBy gw i, i // gw - 1 )


type alias Tile =
    Int


type alias Board =
    { e : GPos, g : Grid Tile }


initialBoard : Board
initialBoard =
    Random.step randomBoard
        (Random.initialSeed 0)
        |> first


boardAsString : Board -> String
boardAsString board =
    --Debug.toString board
    board.g
        |> Grid.toArray
        |> Array.toList
        |> List.map String.fromInt
        |> String.join ","


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
    boardAsString solutionBoard


type alias Node =
    { board : Board
    , boardAsString : String
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
    n.boardAsString == solutionBoardAsString


estimateCostToReachSolution : Board -> Int
estimateCostToReachSolution board =
    totalCellCount - solvedCellCount board


solvedCellCount : Board -> Int
solvedCellCount board =
    board.g
        |> Grid.toArray
        |> Array.toList
        |> List.map2 (\si bi -> si == bi) solutionBoardAsList
        |> List.filter identity
        |> List.length


allDirections =
    [ Up, Down, Left, Right ]


createChildrenNodes : Node -> List Node
createChildrenNodes n =
    allDirections
        |> List.filterMap
            (\dir ->
                slideTileInDirection dir n.board
                    |> Maybe.map
                        (\b ->
                            { board = b
                            , boardAsString = boardAsString b
                            , estimatedCostToReachSolution = estimateCostToReachSolution b
                            , pathToRootCost = n.pathToRootCost + 1
                            , parent = Parent n
                            }
                        )
            )


type alias State =
    { explored : Dict String Node
    , frontier : Frontier
    }


initState : Board -> State
initState b =
    let
        rootNode =
            { board = b
            , boardAsString = boardAsString b
            , estimatedCostToReachSolution = estimateCostToReachSolution b
            , pathToRootCost = 0
            , parent = None
            }
    in
    { explored = Dict.empty
    , frontier = PriorityQueue.empty leastCostOf |> PriorityQueue.insert rootNode
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


solveBoard : Board -> Loop State (Maybe Node)
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



--pop : Frontier -> Maybe ( Node, Frontier )
--pop frontier =
--    let
--        reduce n ( min, acc ) =
--            if leastCostOf n < leastCostOf min then
--                ( n, min :: acc )
--
--            else
--                ( min, n :: acc )
--    in
--    case frontier of
--        [] ->
--            Nothing
--
--        h :: t ->
--            --Just ( h, t )
--            Just (List.foldl reduce ( h, [] ) t)


isExplored : State -> Node -> Bool
isExplored state node =
    let
        _ =
            Debug.log
    in
    Dict.member node.boardAsString state.explored


solveBoardHelp : State -> LoopResult State (Maybe Node)
solveBoardHelp state =
    case pop state.frontier of
        Nothing ->
            Done Nothing

        Just ( node, pendingFrontier ) ->
            if isSolutionNode node then
                Done (Just node)

            else
                let
                    children =
                        createChildrenNodes node

                    filteredChildren =
                        children |> reject (isExplored state)

                    insertNodes =
                        List.foldl (\n -> Dict.insert n.boardAsString n)
                in
                case List.filter isSolutionNode filteredChildren |> List.head of
                    Just c ->
                        Done (Just c)

                    Nothing ->
                        { state
                            | explored = insertNodes state.explored (node :: filteredChildren)
                            , frontier = List.foldl (\c -> PriorityQueue.insert c) pendingFrontier filteredChildren
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
