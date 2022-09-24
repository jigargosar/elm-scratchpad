module TIS100.UI exposing (..)

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


coloredBorder c =
    style "border" ("1px solid " ++ c)


lightBorder =
    coloredBorder lightGray


errorBOrder =
    coloredBorder errorRed


gapSize =
    "5ch"


nodeSize =
    "24ch"


errorRed =
    "red"


fgError =
    fg errorRed
