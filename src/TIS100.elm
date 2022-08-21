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
    in
    div []
        [ div [] [ text <| Debug.toString sim ]
        , viewSim sim
        ]


type Num
    = Num


type Node
    = InputNodeWrapper InputNode


type InputNode
    = InputNode (List Num)


nodeState : Node -> NodeState
nodeState node =
    case node of
        InputNodeWrapper _ ->
            Idle


type NodeState
    = Idle
    | Run
    | Read
    | Write


stepSim : Sim -> Sim
stepSim sim =
    { sim
        | nodes = List.map stepNode sim.nodes
        , cycle = sim.cycle + 1
    }


stepNode : Node -> Node
stepNode node =
    node


type alias Sim =
    { nodes : List Node
    , cycle : Int
    }


initialSim =
    let
        inputNode : InputNode
        inputNode =
            InputNode [ Num ]

        nodeList : List Node
        nodeList =
            [ InputNodeWrapper inputNode ]
    in
    Sim nodeList 0


viewSim sim =
    div [] (List.indexedMap viewNode sim.nodes)


viewNode i node =
    let
        nodeInfo =
            ( i, node, ( "Mode", nodeState node ) )
    in
    div []
        [ text <| Debug.toString nodeInfo
        ]
