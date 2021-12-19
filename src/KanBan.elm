module KanBan exposing (main)

import Utils exposing (..)


main =
    Document "Kanban"
        [ basicStylesNode
        , div [ dGrid, style "grid-auto-flow" "column" ]
            [ div [] [ text "Todo" ]
            , div [] [ text "Ongoing" ]
            , div [] [ text "Done" ]
            ]
        ]
        |> .body
        |> div []
