module TIS100.UI exposing
    ( fgBright
    , fgDull
    , fgError
    , fgNormal
    , gapSize
    , highlightBright
    , highlightError
    , highlightNormal
    , nodeSize
    , outlineError
    , outlineNormal
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
    grayN 0.95


black =
    Utils.black


errorRed =
    "red"



-- OUTLINE


outlineNormal =
    sOutline ("1px solid " ++ lightGray)


outlineError =
    sOutline ("1px solid " ++ errorRed)



-- FG


fgNormal : Attribute msg
fgNormal =
    fg lightGray


fgBright : Attribute msg
fgBright =
    fg white


fgDull : Attribute msg
fgDull =
    fg darkGray


fgError : Attribute msg
fgError =
    fg errorRed



-- COLOR PARINGS


highlightError : List (Attribute msg)
highlightError =
    [ bgc errorRed, fg black ]


highlightNormal =
    [ bgc lightGray, fg black ]


highlightBright =
    [ bgc white, fg black ]
