module TIS100.UI exposing
    ( btn
    , btn2
    , commonStyles
    , fgBright
    , fgDisabled
    , fgDull
    , fgError
    , fgNormal
    , gapSize
    , highlightBright
    , highlightError
    , nodeSize
    , outlineDisabled
    , outlineDull
    , outlineError
    , outlineNormal
    )

import Utils exposing (..)



-- COMMON STYLE NODE


commonStyles : Html msg
commonStyles =
    styleNode """
         :is(textarea, button, input):focus{
             outline:auto!important;
             outline:-webkit-focus-ring-color auto 1px!important;
         }
         body{
            background: black;
         }
         """



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
    grayN 0.55


veryDarkGray =
    grayN 0.4


lightGray : String
lightGray =
    grayN 0.75


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
    sOutline ("2px solid " ++ lightGray)


outlineDull : Attribute msg
outlineDull =
    sOutline ("2px solid " ++ darkGray)


outlineDisabled : Attribute msg
outlineDisabled =
    sOutline ("2px solid " ++ veryDarkGray)


outlineError : Attribute msg
outlineError =
    sOutline ("2px solid " ++ errorRed)



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


fgDisabled : Attribute msg
fgDisabled =
    fg veryDarkGray


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



-- BUTTONS


btn2 : List (Attribute msg) -> Maybe msg -> List (Html msg) -> Html msg
btn2 attrs mbMsg =
    button
        ([ borderNone
         , bgcInherit
         , fgInherit
         , ttu
         , bold
         , ffMonospace
         , fontSizeInherit
         , outlineNormal
         , displayGrid
         , placeContentCenter
         , mbNotifyClick mbMsg
         , pa "0.5ch"
         ]
            ++ attrs
        )


btn : List (Attribute msg) -> Maybe msg -> String -> Html msg
btn attrs mbMsg txt =
    btn2 attrs mbMsg [ text txt ]
