module TIS100.Ports exposing (..)

import Dict exposing (Dict)
import TIS100.IOIntent exposing (IOIntent)
import TIS100.NodeState exposing (NodeState)
import TIS100.Num exposing (Num)
import TIS100.Puzzle exposing (Puzzle)


type Ports
    = Ports PortsDict


type alias PortsDict =
    Dict PortKey PortValue


type alias Addr =
    ( Int, Int )


type alias PortKey =
    ( Addr, Addr )


type PortValue
    = Empty
    | Num Num
    | Queried


init : Ports
init =
    Debug.todo "todo"


fromPuzzle : Puzzle -> Ports
fromPuzzle puzzle =
    Debug.todo "todo"


fromIOIntentsAndNodeState : Puzzle -> List ( IOIntent, NodeState a ) -> Ports
fromIOIntentsAndNodeState puzzle list =
    Debug.todo "todo"
