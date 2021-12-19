module KanBan exposing (main)

import Utils exposing (..)


main =
    Document "Kanban"
        [ basicStylesNode
        , div [ pa "20px", gap "20px", dGrid, style "grid-auto-flow" "column" ]
            (bucketTitles
                |> List.map viewColumn
            )
        ]
        |> .body
        |> div []


bucketTitles =
    [ "Todo"
    , "Ongoing"
    , "Done"
    ]


type alias Task =
    { title : String
    }


type alias Bucket =
    { title : String
    , items : List Task
    }


viewColumn t =
    div []
        [ div [ fontSize "22px", ttu, bold ] [ text t ]
        , div [ fontSize "18px", fg (grayN 0.4) ] [ text "1 items" ]
        ]
