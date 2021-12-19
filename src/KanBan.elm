module KanBan exposing (main)

import Utils exposing (..)


main =
    Document "Kanban"
        [ basicStylesNode
        , div [ dGrid, style "grid-auto-flow" "column" ]
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
    div [ ttu ] [ text t ]
