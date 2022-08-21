module TIS100 exposing (main)

import Dict exposing (Dict)
import TIS100.InputNode as InputNode exposing (InputNode)
import TIS100.Num as Num exposing (Num)
import TIS100.OutputNode as OutputNode exposing (OutputNode)
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
                --|> stepSim
                --|> stepSim
                --|> stepSim
                --|> stepSim
                --|> stepSim
                |> stepSim
                |> identity
    in
    div []
        [ div [] [ text <| Debug.toString sim ]
        , viewSim sim
        ]


type Node
    = InputNode InputNode
    | OutputNode OutputNode


stepSim : Sim -> Sim
stepSim sim =
    { sim
        | nodeStore = stepSimHelp sim.nodeStore
        , cycle = sim.cycle + 1
    }


stepSimHelp : NodeStore -> NodeStore
stepSimHelp ns =
    let
        toNodeStore : SimAcc -> NodeStore
        toNodeStore { writeBlocked, completed } =
            Dict.union writeBlocked completed
    in
    let
        ( writeBlocked, pending ) =
            Dict.partition (\_ -> isWriteBlocked) ns

        initialAcc : SimAcc
        initialAcc =
            { writeBlocked = writeBlocked
            , completed = Dict.empty
            }

        stepper addr node acc =
            let
                readFn =
                    initReadFn addr acc.writeBlocked
            in
            updateSimAcc addr (stepNode readFn node) acc
    in
    Dict.foldl stepper initialAcc pending |> toNodeStore


type alias SimAcc =
    { writeBlocked : NodeStore
    , completed : NodeStore
    }


updateSimAcc : NodeAddr -> ( Node, Maybe NodeEntry ) -> SimAcc -> SimAcc
updateSimAcc addr ( node, mbEntry ) acc =
    acc
        |> simAccAdd ( addr, node )
        |> simAccAddMaybe mbEntry


simAccAdd : NodeEntry -> SimAcc -> SimAcc
simAccAdd ( na, n ) acc =
    { acc
        | writeBlocked = acc.writeBlocked |> Dict.remove na
        , completed = acc.completed |> Dict.insert na n
    }


simAccAddMaybe : Maybe NodeEntry -> SimAcc -> SimAcc
simAccAddMaybe mbEntry acc =
    case mbEntry of
        Just entry ->
            simAccAdd entry acc

        Nothing ->
            acc


type alias ReadFn a =
    () -> Maybe ( Num, a )


addrUp : NodeAddr -> NodeAddr
addrUp ( x, y ) =
    ( x, y - 1 )


initReadFn : NodeAddr -> NodeStore -> ReadFn NodeEntry
initReadFn addr p () =
    let
        readFromAddr : NodeAddr
        readFromAddr =
            addrUp addr
    in
    Dict.get readFromAddr p
        |> Maybe.andThen
            (\n ->
                readNode n
                    |> Maybe.map (mapSecond (pair readFromAddr))
            )


stepNode : ReadFn a -> Node -> ( Node, Maybe a )
stepNode readFn node =
    case node of
        InputNode inputNode ->
            ( InputNode (InputNode.step inputNode), Nothing )

        OutputNode outputNode ->
            OutputNode.step readFn outputNode
                |> mapFirst OutputNode


readNode : Node -> Maybe ( Num, Node )
readNode node =
    case node of
        InputNode inputNode ->
            InputNode.read inputNode
                |> Maybe.map (mapSecond InputNode)

        OutputNode _ ->
            Nothing


isWriteBlocked : Node -> Bool
isWriteBlocked =
    readNode >> maybeToBool


type alias NodeAddr =
    ( Int, Int )


type alias NodeStore =
    Dict NodeAddr Node


type alias NodeEntry =
    ( NodeAddr, Node )


type alias Sim =
    { nodeStore : NodeStore
    , cycle : Int
    }


initialSim : Sim
initialSim =
    let
        inputNode : InputNode
        inputNode =
            InputNode.fromList (List.repeat 3 Num.zero)

        outputNode : OutputNode
        outputNode =
            OutputNode.fromExpected 3

        nodeList : List Node
        nodeList =
            [ InputNode inputNode, OutputNode outputNode ]

        nodeStore : NodeStore
        nodeStore =
            nodeList
                |> List.indexedMap pair
                |> List.map (mapFirst (pair 0))
                |> Dict.fromList
    in
    { nodeStore = nodeStore, cycle = 0 }


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
