module KanBan exposing (main)

import Utils exposing (..)


main =
    Document "Kanban"
        [ basicStylesNode
        , div []
            [ div [] [ text "TODO" ]
            , div [] [ text "Ongoing" ]
            , div [] [ text "Done" ]
            ]
        ]
        |> .body
        |> div []
