module KanBan exposing (main)

import Utils exposing (..)


main =
    Document "Kanban"
        [ basicStylesNode
        , div [ pa "20px", gap "20px", dGrid, style "grid-auto-flow" "column" ]
            (buckets
                |> List.map viewBucketColumn
            )
        ]
        |> .body
        |> div []


buckets =
    bucketTitles
        |> List.indexedMap
            (\bi t ->
                Bucket t
                    (List.range 0 3
                        |> List.map (\ti -> Task ("Demo Task #" ++ fromInt (bi * ti)))
                        |> List.take (bi + 2)
                    )
            )


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


viewBucketColumn : Bucket -> Html msg
viewBucketColumn b =
    fCol [ gap "10px" ]
        [ div []
            [ div [ fontSize "22px", ttu, bold ] [ text b.title ]
            , div [ fontSize "18px", fg (grayN 0.4) ]
                [ text <| fromInt (List.length b.items) ++ " items" ]
            ]
        , fCol [ gap "10px" ]
            (List.map viewTaskItem b.items)
        ]


viewTaskItem : Task -> Html msg
viewTaskItem t =
    div [] [ text t.title ]
