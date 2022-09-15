module TIS100.PuzzlePage.Scratch exposing (..)

import Html exposing (input, pre, textarea)
import TIS100.UI as UI
import Utils exposing (..)


main : Html.Html msg
main =
    div []
        [ basicStylesNode
        , fCol
            [ h100
            , fontSize "12px"
            , styleLineHeight "0.9"
            , pa "3ch"
            , bold
            , ffMonospace
            , gap "2ch"
            , ttu
            ]
            [ div
                [ displayGrid
                , sWidth "18ch"
                , sHeight UI.nodeSize
                , positionRelative
                ]
                [ pre
                    [ positionAbsolute
                    , pa "0.5ch"
                    ]
                    [ text "  "
                    , span
                        [ textDecoration "underline wavy red"
                        ]
                        [ text "editor" ]
                    ]
                , viewEditor
                ]
            ]
        ]


textDecoration =
    style "text-decoration"


viewEditor =
    Html.textarea
        [ -- reset
          borderNone
        , outlineNone
        , resizeNone

        -- inherit
        , bgcInherit
        , fgInherit
        , ttInherit
        , fontInherit

        --, onInput onInputMsg
        -- actual
        , sOutline "1px solid white"
        , pa "0.5ch"
        ]
        [ text "  editor" ]
