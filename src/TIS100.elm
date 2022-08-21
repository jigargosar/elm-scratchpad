module TIS100 exposing (main)

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


type OutputNode
    = ON_Idle (List Num)
    | ON_Run Int (List Num)
    | ON_Read Int (List Num)


outputNodeInit : Int -> OutputNode
outputNodeInit expected =
    ON_Run expected []


outputNodeStep : OutputNode -> OutputNode
outputNodeStep node =
    case node of
        ON_Idle _ ->
            node

        ON_Run int nums ->
            ON_Read int nums

        ON_Read _ _ ->
            node


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


stepSim : Sim -> Sim
stepSim sim =
    { sim
        | nodes = List.map stepNode sim.nodes
        , cycle = sim.cycle + 1
    }


stepNode : Node -> Node
stepNode node =
    case node of
        INW inputNode ->
            INW (inputNodeStep inputNode)

        ONW outputNode ->
            ONW (outputNodeStep outputNode)


type alias Sim =
    { nodes : List Node
    , cycle : Int
    }


initialSim =
    let
        inputNode : InputNode
        inputNode =
            inputNodeInitFromList [ Num, Num, Num ]

        outputNode : OutputNode
        outputNode =
            outputNodeInit 3

        nodeList : List Node
        nodeList =
            [ INW inputNode, ONW outputNode ]
    in
    Sim nodeList 0


viewSim sim =
    div [] (List.indexedMap viewNode sim.nodes)


viewNode i node =
    let
        nodeInfo =
            ( i, node )
    in
    div []
        [ text <| Debug.toString nodeInfo
        ]
