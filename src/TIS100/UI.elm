module TIS100.UI exposing
    ( fgBright
    , fgDark
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


gray =
    grayN 0.5


offWhite =
    grayN 0.7


white =
    grayN 0.95


black =
    Utils.black


errorRed =
    "red"



-- OUTLINE


outlineNormal =
    sOutline ("1px solid " ++ offWhite)


outlineError =
    sOutline ("1px solid " ++ errorRed)



-- FG


fgNormal : Attribute msg
fgNormal =
    fg offWhite


fgBright : Attribute msg
fgBright =
    fg white


fgDark : Attribute msg
fgDark =
    fg gray


fgError : Attribute msg
fgError =
    fg errorRed



-- COLOR PARINGS


highlightError : List (Attribute msg)
highlightError =
    [ bgc errorRed, fg black ]


highlightNormal =
    [ bgc offWhite, fg black ]


highlightBright =
    [ bgc white, fg black ]
