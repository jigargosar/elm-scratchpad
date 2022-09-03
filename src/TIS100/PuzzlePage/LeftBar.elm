module TIS100.PuzzlePage.LeftBar exposing (..)

import TIS100.Num as Num exposing (Num)
import TIS100.SelectionList as SelectionList exposing (SelectionList)
import TIS100.UI as UI
import Utils exposing (..)


type alias LeftBarViewModel =
    { inputs : List InputColumnViewModel
    , outputs : List OutputColumnViewModel
    }


type alias InputColumnViewModel =
    { title : String, nums : SelectionList Num }


type alias OutputColumnViewModel =
    { title : String
    , expected : SelectionList Num
    , actual : List Num
    }


type Msg
    = STOP
    | STEP
    | RUN
    | FAST


viewLeftBar : LeftBarViewModel -> Html Msg
viewLeftBar model =
    fCol [ sWidth "40ch", gap "2ch", fg UI.lightGray ]
        [ div [] [ viewTitle, viewDesc ]
        , fRow [ tac, gap "2ch" ]
            (List.map viewInputColumn model.inputs
                ++ List.map viewOutputColumn model.outputs
            )
        , viewButtons
        ]


viewButtons : Html Msg
viewButtons =
    gtCols 4
        [ gap "2ch" ]
        [ btn "stop" STOP
        , btn "step" STEP
        , btn "run" RUN
        , btn "fast" FAST
        ]


btn : String -> msg -> Html msg
btn txt msg =
    button
        [ UI.lightOutline
        , bgc "inherit"
        , fg "inherit"
        , style "text-transform" "inherit"
        , style "font" "inherit"
        , borderNone
        , dGrid
        , placeContentCenter
        , aspectRatio "1"
        , notifyClick msg
        ]
        [ text txt ]


viewTitle : Html msg
viewTitle =
    div [ tac, styleLineHeight "2" ] [ text "-- Title --" ]


viewDesc : Html msg
viewDesc =
    fCol
        [ UI.lightOutline
        , pa "0.5ch"
        , placeContentCenter
        ]
        (List.repeat 6 (div [] [ text "> desc" ]))


viewInputColumn : InputColumnViewModel -> Html msg
viewInputColumn { title, nums } =
    fCol [ gap "0.5ch" ]
        [ div [] [ text title ]
        , viewNumColumn (Num.viewSelectionList nums)
        ]


viewOutputColumn : OutputColumnViewModel -> Html msg
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
        [ UI.lightOutline
        , sWidth "4ch"
        , pa "0.5ch 0"
        , styleLineHeight "0.8"
        ]
        (numViews
            ++ List.repeat 39 (div [] [ text nbsp ])
            |> List.take 39
        )
