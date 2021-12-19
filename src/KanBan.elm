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
                    (rangeStartSize ((bi * 4) + 1) 4
                        |> List.map (\ti -> Task ("Demo Task #" ++ fromInt ti))
                        |> List.take (bi + 2)
                    )
            )


rangeStartSize s sz =
    List.range s (s + sz)


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
    div [ bgc black, pa "5px", style "border-radius" "5px" ] [ text t.title ]
