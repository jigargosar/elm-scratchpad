module TIS100.PuzzlePage.MockEditorError exposing (..)

import Html exposing (pre)
import Html.Attributes
import TIS100.PuzzlePage.CompilerV2 as Compiler exposing (ErrorRecord)
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
                    , noPointerEvents
                    , fg transparent
                    ]
                    errorText
                , viewEditor
                ]
            ]
        ]


errorText : List (Html msg)
errorText =
    case compilerResult of
        Err errors ->
            List.map viewError errors

        Ok _ ->
            []


viewError : ErrorRecord -> Html msg
viewError error =
    span [ positionAbsolute ]
        [ text (String.repeat (error.row - 1) "\n")
        , text (String.repeat (error.startCol - 1) " ")
        , span
            [ textDecoration "underline 1px solid red"
            ]
            [ text (String.repeat (error.endCol - error.startCol) " ") ]
        ]


compilerResult : Result (List ErrorRecord) ()
compilerResult =
    editorText
        |> Compiler.compileInternal
        |> Result.mapError
            (List.filterMap
                (\( l, e ) ->
                    case e of
                        Compiler.InvalidOp col string ->
                            Just
                                { row = l
                                , startCol = col
                                , endCol = col + String.length string
                                , msg = "Invalid op:\"" ++ string ++ "\""
                                }

                        _ ->
                            Nothing
                )
            )


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
