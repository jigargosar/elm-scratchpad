module TIS100.Ports exposing (..)

import Dict exposing (Dict)
import TIS100.Num exposing (Num)


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
