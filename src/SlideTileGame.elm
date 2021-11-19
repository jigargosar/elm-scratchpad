module SlideTileGame exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, text)
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
    15 * 1000


gw =
    3


gh =
    3


totalCellCount =
    gw * gh


cz =
    160


gpToWorld : GPos -> Vec
gpToWorld =
    vFromGP >> vScale cz >> vAdd1 (cz / 2)


type alias Model =
    { board : Board
    , loop : Loop PriorityQueue (Maybe Node)
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


viewLoop : Loop PriorityQueue (Maybe Node) -> Html Msg
viewLoop loop =
    case loop of
        Complete (Just (Node n)) ->
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
                    ++ " iterations"


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
    board.d
        |> Dict.toList
        |> List.map viewTile
        |> group []


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
        slideInDirections : List Dir4 -> Board
        slideInDirections =
            List.foldl (\dir -> withRollback (slideTileInDirection dir)) solutionBoard
    in
    Random.list 100 randomDir
        |> Random.map slideInDirections


slideTileInDirection : Dir4 -> Board -> Maybe Board
slideTileInDirection dir board =
    moveTileAt (moveInDir4 (oppositeDir4 dir) board.e) board


solutionBoard : Board
solutionBoard =
    let
        gps =
            rangeWH gw gh
                |> List.take (totalCellCount - 1)

        d =
            gps
                |> List.indexedMap (\i gp -> ( gp, i ))
                |> Dict.fromList
    in
    { e = ( gw - 1, gh - 1 ), d = d }


type Node
    = Node
        { board : Board
        , boardStringRepresentation : String
        , estimatedCostToReachSolution : Int
        , pathToRootCost : Int
        , parent : Maybe Node
        }


leastCostOf : Node -> Int
leastCostOf (Node n) =
    n.pathToRootCost + n.estimatedCostToReachSolution


isSolutionNode : Node -> Bool
isSolutionNode (Node n) =
    n.board == solutionBoard


possibleNextBoards : Board -> List Board
possibleNextBoards board =
    [ Up, Down, Left, Right ]
        |> List.filterMap (\dir -> slideTileInDirection dir board)


estimateCostToReachSolution : Board -> Int
estimateCostToReachSolution board =
    totalCellCount - solvedCellCount board


solvedCellCount : Board -> Int
solvedCellCount board =
    Dict.merge (\_ _ -> identity)
        (\_ a b ->
            if a == b then
                inc

            else
                identity
        )
        (\_ _ -> identity)
        solutionBoard.d
        board.d
        (if solutionBoard.e == board.e then
            1

         else
            0
        )


createChildrenNodes : Node -> List Node
createChildrenNodes ((Node p) as parent) =
    possibleNextBoards p.board
        |> List.filterMap
            (\b ->
                let
                    boardStringRepresentation =
                        Debug.toString b

                    isCircular (Node ancestor) =
                        if ancestor.boardStringRepresentation == boardStringRepresentation then
                            True

                        else
                            case ancestor.parent of
                                Just ga ->
                                    isCircular ga

                                Nothing ->
                                    False
                in
                if isCircular parent then
                    Nothing

                else
                    Just
                        (Node
                            { board = b
                            , boardStringRepresentation = boardStringRepresentation
                            , estimatedCostToReachSolution = estimateCostToReachSolution b
                            , pathToRootCost = p.pathToRootCost + 1
                            , parent = Just parent
                            }
                        )
            )


initRootNode : Board -> Node
initRootNode b =
    Node
        { board = b
        , boardStringRepresentation = Debug.toString b
        , estimatedCostToReachSolution = estimateCostToReachSolution b
        , pathToRootCost = 0
        , parent = Nothing
        }


type PriorityQueue
    = PriorityQueue (List Node)


priorityQueueFrom : Node -> PriorityQueue
priorityQueueFrom node =
    PriorityQueue [ node ]


enqueueAll : List Node -> PriorityQueue -> PriorityQueue
enqueueAll candidates (PriorityQueue live) =
    PriorityQueue (live ++ candidates)


dequeue : PriorityQueue -> Maybe ( Node, PriorityQueue )
dequeue (PriorityQueue live) =
    case List.sortBy leastCostOf live of
        [] ->
            Nothing

        n :: rest ->
            Just ( n, PriorityQueue rest )


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


solveBoard : Board -> Loop PriorityQueue (Maybe Node)
solveBoard board =
    initRootNode board
        |> priorityQueueFrom
        |> Looping
        |> stepLoopN maxIterations solveBoardHelp


solveBoardHelp : PriorityQueue -> LoopResult PriorityQueue (Maybe Node)
solveBoardHelp pq =
    case dequeue pq of
        Nothing ->
            Done Nothing

        Just ( node, pendingPQ ) ->
            if isSolutionNode node then
                Done (Just node)

            else
                Loop (enqueueAll (createChildrenNodes node) pendingPQ)


moveTileAt : GPos -> Board -> Maybe Board
moveTileAt gp board =
    case ( Dict.get gp board.d, areAdjacent board.e gp ) of
        ( Just gpTile, True ) ->
            Just
                { board
                    | e = gp
                    , d =
                        board.d
                            |> Dict.remove gp
                            |> Dict.insert board.e gpTile
                }

        _ ->
            Nothing


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
