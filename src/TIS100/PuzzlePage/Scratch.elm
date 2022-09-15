module TIS100.PuzzlePage.Scratch exposing (..)

import Html exposing (input, pre, textarea)
import Html.Attributes
import List.Extra
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
                    , w100
                    , h100
                    , overflowClip
                    , noPointerEvents
                    , fg transparent
                    ]
                    errorText
                , viewEditor
                ]
            ]
        ]


errorText =
    String.lines editorText
        |> List.map
            (\l ->
                String.split " " l
                    |> List.map
                        (\w ->
                            span
                                [ textDecoration "underline 1px solid red"
                                ]
                                [ text w ]
                        )
                    |> List.intersperse (text " ")
            )
        |> List.intersperse [ text "\n" ]
        |> List.concat


editorText =
    """
    all characters
except whitespaces
should be error
underlined
"""


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
        , Html.Attributes.spellcheck False
        , sOutline "1px solid white"
        , pa "0.5ch"
        , whiteSpace "pre"
        , overflowClip
        , w100
        , h100
        , style "scroll-left" "0"
        ]
        [ text editorText ]
