module TIS100.PuzzlePage.LeftBar exposing (..)

import TIS100.Num as Num exposing (Num)
import TIS100.SelectionList as SelectionList exposing (SelectionList)
import TIS100.UI as UI
import Utils exposing (..)


type alias ViewModel =
    { title : String
    , description : List String
    , inputs : List Input
    , outputs : List Output
    }


type alias Config msg =
    { stop : Maybe msg
    , step : Maybe msg
    , run : Maybe msg
    , fast : Maybe msg
    }


type alias Input =
    { title : String, nums : SelectionList Num }


type alias Output =
    { title : String
    , expected : SelectionList Num
    , actual : List Num
    }


view : Config msg -> ViewModel -> Html msg
view conf model =
    fCol [ sWidth "40ch", gap "2ch", UI.fgNormal ]
        [ div [] [ viewTitle model.title, viewDesc model.description ]
        , fRow [ tac, gap "2ch" ]
            (List.map viewInputColumn model.inputs
                ++ List.map viewOutputColumn model.outputs
            )
        , viewButtons conf
        ]


viewButtons { stop, step, run, fast } =
    gtCols 4
        [ gap "2ch" ]
        [ btn "stop" stop
        , btn "step" step
        , btn "run" run
        , btn "fast" fast
        ]


btn : String -> Maybe msg -> Html msg
btn txt onClick =
    button
        [ bgcInherit
        , fgInherit
        , ttInherit
        , fontInherit
        , UI.outlineNormal
        , borderNone
        , displayGrid
        , placeContentCenter
        , aspectRatio "1"
        , mbNotifyClick onClick
        ]
        [ text txt ]


viewTitle : String -> Html msg
viewTitle title =
    div [ tac, styleLineHeight "2" ]
        [ text "-- "
        , text title
        , text " --"
        ]


viewDesc ls =
    div [ UI.outlineNormal, pa "0.5ch" ]
        [ fCol [ sHeight "6em" ]
            (ls
                |> List.take 6
                |> List.map viewDescLine
            )
        ]


viewDescLine l =
    fRow []
        [ div [] [ text ">", text nbsp ]
        , div [] [ text l ]
        ]


viewInputColumn : Input -> Html msg
viewInputColumn { title, nums } =
    fCol [ gap "0.5ch" ]
        [ div [] [ text title ]
        , viewNumColumn (Num.viewSelectionList nums)
        ]


viewOutputColumn : Output -> Html msg
viewOutputColumn { title, expected, actual } =
    fCol [ gap "0.5ch" ]
        [ div [] [ text title ]
        , fRow []
            [ viewNumColumn (Num.viewSelectionList expected)
            , viewNumColumn
                (List.map2
                    (\e a ->
                        if e == a then
                            Num.view a

                        else
                            Num.viewError a
                    )
                    (SelectionList.toList expected)
                    actual
                )
            ]
        ]


viewNumColumn : List (Html msg) -> Html msg
viewNumColumn numViews =
    div
        [ UI.outlineNormal
        , sWidth "4ch"
        , pa "0.5ch 0"
        , styleLineHeight "0.8"
        ]
        (numViews
            ++ List.repeat 39 (div [] [ text nbsp ])
            |> List.take 39
        )
