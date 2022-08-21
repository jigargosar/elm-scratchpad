module TIS100 exposing (main)

import Dict exposing (Dict)
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = viewDocument
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


type Msg
    = NOP


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )


viewDocument : Model -> Document Msg
viewDocument _ =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, text "BrowserDocumentTemplate"
        , view
        ]


view =
    let
        sim =
            initialSim
                |> stepSim
                |> stepSim
                |> stepSim
                |> stepSim
                |> stepSim
                |> stepSim
                |> identity
    in
    div []
        [ div [] [ text <| Debug.toString sim ]
        , viewSim sim
        ]


type Num
    = Num


type Node
    = INW InputNode
    | ONW OutputNode


type InputNode
    = IN_Idle
    | IN_Run Num (List Num)
    | IN_Write Num (List Num)


inputNodeInitFromList : List Num -> InputNode
inputNodeInitFromList nums =
    case nums of
        f :: r ->
            IN_Run f r

        [] ->
            IN_Idle


inputNodeStep : InputNode -> InputNode
inputNodeStep node =
    case node of
        IN_Run n ns ->
            IN_Write n ns

        IN_Write _ _ ->
            node

        IN_Idle ->
            node


inputNodeRead : InputNode -> Maybe ( Num, InputNode )
inputNodeRead node =
    case node of
        IN_Idle ->
            Nothing

        IN_Run _ _ ->
            Nothing

        IN_Write num nums ->
            Just ( num, inputNodeInitFromList nums )


type OutputNode
    = ON_Idle (List Num)
    | ON_Run Int (List Num)
    | ON_Read Int (List Num)


outputNodeInit : Int -> OutputNode
outputNodeInit expected =
    if expected <= 0 then
        ON_Idle []

    else
        ON_Run expected []


outputNodeStep : (() -> Maybe ( Num, a )) -> OutputNode -> ( OutputNode, Maybe a )
outputNodeStep readFn node =
    let
        attemptRead int nums =
            case readFn () of
                Nothing ->
                    ( ON_Read int nums, Nothing )

                Just ( num, a ) ->
                    ( ON_Run (int - 1) (num :: nums), Just a )
    in
    case node of
        ON_Idle _ ->
            ( node, Nothing )

        ON_Run int nums ->
            attemptRead int nums

        ON_Read int nums ->
            attemptRead int nums


stepSim : Sim -> Sim
stepSim sim =
    { sim
        | nodeStore = stepNodeStore sim.nodeStore
        , cycle = sim.cycle + 1
    }


stepNodeStore : NodeStore -> NodeStore
stepNodeStore ns =
    let
        removeInsert ( k, v ) ( p, n ) =
            ( Dict.remove k p, Dict.insert k v n )

        removeInsertMaybe mb val =
            Maybe.map (\kv -> removeInsert kv val) mb
                |> Maybe.withDefault val

        ret =
            Dict.foldl
                (\k v (( p, c ) as acc) ->
                    case readNode v of
                        Just _ ->
                            acc

                        Nothing ->
                            let
                                _ =
                                    Debug.log "Debug: " p

                                readFn () =
                                    let
                                        kPrev : NodeAddr
                                        kPrev =
                                            (k - 1)
                                                |> Debug.log "Debug: "
                                    in
                                    Dict.get kPrev (Debug.log "Debug: " p)
                                        |> Maybe.andThen
                                            (\n ->
                                                readNode n
                                                    |> Maybe.map (mapSecond (pair kPrev))
                                            )

                                ( nv, mbNKV ) =
                                    stepNode readFn v
                            in
                            acc
                                |> removeInsert ( k, nv )
                                |> removeInsertMaybe mbNKV
                )
                ( ns, Dict.empty )
                ns
                |> (\( p, c ) -> Dict.union p c)
    in
    ret


stepNode : (() -> Maybe ( Num, a )) -> Node -> ( Node, Maybe a )
stepNode readFn node =
    case node of
        INW inputNode ->
            ( INW (inputNodeStep inputNode), Nothing )

        ONW outputNode ->
            outputNodeStep readFn outputNode
                |> mapFirst ONW


readNode : Node -> Maybe ( Num, Node )
readNode node =
    case node of
        INW inputNode ->
            inputNodeRead inputNode
                |> Maybe.map (mapSecond INW)

        ONW _ ->
            Nothing


type alias NodeAddr =
    Int


type alias NodeStore =
    Dict NodeAddr Node


type alias NodeEntry =
    ( NodeAddr, Node )


type alias Sim =
    { nodeStore : NodeStore
    , cycle : Int
    }


initialSim =
    let
        inputNode : InputNode
        inputNode =
            inputNodeInitFromList [ Num, Num, Num ]

        outputNode : OutputNode
        outputNode =
            outputNodeInit 1

        nodeList : List Node
        nodeList =
            [ INW inputNode, ONW outputNode ]

        nodeStore : NodeStore
        nodeStore =
            nodeList |> List.indexedMap pair |> Dict.fromList
    in
    Sim nodeStore 0


viewSim : Sim -> Html Msg
viewSim sim =
    div [] (Dict.toList sim.nodeStore |> List.map viewNode)


viewNode ( nodeAddr, node ) =
    let
        nodeInfo =
            ( nodeAddr, node )
    in
    div []
        [ text <| Debug.toString nodeInfo
        ]
