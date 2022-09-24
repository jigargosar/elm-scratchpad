module TIS100.UI exposing (..)

-- UI HELPERS

import Utils exposing (..)


darkGray =
    grayN 0.5


lightGray =
    grayN 0.7


coloredOutline c =
    sOutline ("1px solid " ++ c)


lightOutline =
    coloredOutline lightGray


errorOutline =
    coloredOutline errorRed


gapSize =
    "5ch"


nodeSize =
    "24ch"


errorRed =
    "red"


fgError =
    fg errorRed
