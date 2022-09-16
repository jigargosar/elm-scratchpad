module TIS100.PuzzlePage.MockEditorError exposing (..)

import Html exposing (pre)
import Html.Attributes
import TIS100.PuzzlePage.CompilerV2 as Compiler
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
            (\string ->
                case Compiler.compileLine string of
                    Err (Compiler.InvalidOp col token) ->
                        List.repeat (col - 1) (text " ")
                            ++ [ span
                                    [ textDecoration "underline 1px solid red"
                                    ]
                                    [ text token ]
                               ]

                    _ ->
                        [ text "" ]
            )
        |> List.intersperse [ text "\n" ]
        |> List.concat


editorText =
    """
nop # valid
nop: nop #valid
 foo #invalid
: #invalid
#valid
foo: bar #invalid
"""
        |> String.lines
        |> List.drop 1
        |> String.join "\n"


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
