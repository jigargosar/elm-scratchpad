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
             --String.split " " l
             --    |> List.map
             --        (\w ->
             --            span
             --                [ textDecoration "underline 1px solid red"
             --                ]
             --                [ text w ]
             --        )
             --    |> List.intersperse (text " ")
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
        |> String.replace "\u{000D}" ""



--|> tapBy String.length
--noinspection ElmUnusedSymbol


tapBy fn e =
    let
        _ =
            Debug.log "tap: " (fn e)
    in
    e


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
