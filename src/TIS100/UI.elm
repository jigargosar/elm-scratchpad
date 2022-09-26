module TIS100.UI exposing
    ( fgBright
    , fgDull
    , fgError
    , fgNormal
    , gapSize
    , highlightBright
    , highlightError
    , nodeSize
    , outlineError
    , outlineNormal
    )

import Utils exposing (..)



-- SIZES


gapSize : String
gapSize =
    "5ch"


nodeSize : String
nodeSize =
    "24ch"



-- COLORS


darkGray : String
darkGray =
    grayN 0.5


lightGray : String
lightGray =
    grayN 0.7


white : String
white =
    grayN 0.95


black : String
black =
    Utils.black


errorRed : String
errorRed =
    "red"



-- OUTLINE


outlineNormal : Attribute msg
outlineNormal =
    sOutline ("1px solid " ++ lightGray)


outlineError : Attribute msg
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


highlightBright : List (Attribute msg)
highlightBright =
    [ bgc white, fg black ]
