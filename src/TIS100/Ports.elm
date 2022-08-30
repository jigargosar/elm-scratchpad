module TIS100.Ports exposing (..)

import Dict exposing (Dict)
import TIS100.IOIntent exposing (IOIntent(..))
import TIS100.Num exposing (Num)
import Utils exposing (Dir4, moveInDir4, oppositeDir4)


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
