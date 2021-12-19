module KanBan exposing (main)

import Utils exposing (..)


main =
    Document "Kanban"
        [ basicStylesNode
        , div [ pa "20px", gap "20px", dGrid, style "grid-auto-flow" "column" ]
            ([ "Todo"
             , "Ongoing"
             , "Done"
             ]
                |> List.map viewColumn
            )
        ]
        |> .body
        |> div []


viewColumn t =
    div []
        [ div [ ttu, bold ] [ text t ]
        , div [ style "color" <| grayN 0.4 ] [ text "1 items" ]
        ]
