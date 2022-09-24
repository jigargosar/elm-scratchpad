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


bright =
    grayN 0.95


dark =
    Utils.black


error =
    "red"



-- OUTLINE


outlineNormal =
    sOutline ("1px solid " ++ offWhite)


outlineError =
    sOutline ("1px solid " ++ error)



-- FG


fgNormal : Attribute msg
fgNormal =
    fg offWhite


fgBright : Attribute msg
fgBright =
    fg bright


fgDark : Attribute msg
fgDark =
    fg gray


fgError : Attribute msg
fgError =
    fg error



-- COLOR PARINGS


highlightError : List (Attribute msg)
highlightError =
    [ bgc error, fg dark ]


highlightNormal =
    [ bgc offWhite, fg dark ]


highlightBright =
    [ bgc bright, fg dark ]
