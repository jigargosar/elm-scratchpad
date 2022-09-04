module TIS100.UI exposing (..)

-- UI HELPERS

import Utils exposing (..)


darkGray =
    grayN 0.5


lightGray =
    grayN 0.7


lightOutline =
    sOutline ("1px solid " ++ lightGray)


gapSize =
    "5ch"


nodeSize =
    "24ch"


errorRed =
    "red"


type alias Addr =
    Int2


gridAreaFromAddr : Addr -> Attribute msg
gridAreaFromAddr ( x, y ) =
    gridAreaXY ( x, y - 1 )
