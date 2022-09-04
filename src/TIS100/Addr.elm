module TIS100.Addr exposing (..)

import Utils as U exposing (Attribute, Int2, Num2)


type alias Addr =
    Int2


toGridArea : Addr -> Attribute msg
toGridArea ( x, y ) =
    U.gridAreaXY ( x, y - 1 )


inputGridArea : Int -> Attribute msg
inputGridArea x =
    U.gridAreaXY ( x, 0 )


outputGridArea : Int -> Attribute msg
outputGridArea x =
    U.gridAreaXY ( x, 2 )
