module TIS100.UI exposing
    ( errorOutline
    , errorRed
    , fgBright
    , fgDark
    , fgError
    , fgNormal
    , gapSize
    , lightGray
    , lightOutline
    , nodeSize
    )

import Utils exposing (..)



-- SIZES


gapSize =
    "5ch"


nodeSize =
    "24ch"



-- COLORS


darkGray =
    grayN 0.5


lightGray =
    grayN 0.7


white =
    Utils.white


errorRed =
    "red"



-- OUTLINE


lightOutline =
    sOutline ("1px solid " ++ lightGray)


errorOutline =
    sOutline ("1px solid " ++ errorRed)



-- FG


fgNormal : Attribute msg
fgNormal =
    fg lightGray


fgBright : Attribute msg
fgBright =
    fg white


fgDark : Attribute msg
fgDark =
    fg darkGray


fgError : Attribute msg
fgError =
    fg errorRed
