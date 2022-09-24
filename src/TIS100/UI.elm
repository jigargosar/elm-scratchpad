module TIS100.UI exposing (..)

import Utils exposing (..)


darkGray =
    grayN 0.5


lightGray =
    grayN 0.7


lightOutline =
    sOutline ("1px solid " ++ lightGray)


errorOutline =
    sOutline ("1px solid " ++ errorRed)


gapSize =
    "5ch"


nodeSize =
    "24ch"


errorRed =
    "red"


fgError =
    fg errorRed
