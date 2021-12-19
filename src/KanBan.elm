module KanBan exposing (main)

import Utils exposing (..)


main =
    Document "Kanban"
        [ basicStylesNode
        , stylesNode """
            body{
                background-color: #333;
            }
        """
        , div
            [ pa "20px"
            , gap "20px"
            , dGrid
            , style "grid-auto-flow" "column"
            ]
            (buckets
                |> List.map viewBucketColumn
            )
        ]
        |> .body
        |> div []


buckets =
    emptyBuckets
        |> List.indexedMap
            (\bi b ->
                { b
                    | items =
                        rangeStartSize ((bi * 4) + 1) 4
                            |> List.map (\ti -> Task ("Demo Task #" ++ fromInt ti))
                            |> List.take (bi + 2)
                }
            )


rangeStartSize s sz =
    List.range s (s + sz)


emptyBuckets : List Bucket
emptyBuckets =
    [ Bucket "Todo" "green" []
    , Bucket "Ongoing" "green" []
    , Bucket "Done" "green" []
    ]


type alias Task =
    { title : String
    }


type alias Bucket =
    { title : String
    , color : String
    , items : List Task
    }


viewBucketColumn : Bucket -> Html msg
viewBucketColumn b =
    fCol [ gap "20px" ]
        [ div []
            [ div [ fontSize "22px", ttu, bold ] [ text b.title ]
            , div [ fontSize "18px", fg (grayN 0.6) ]
                [ text <| fromInt (List.length b.items) ++ " items" ]
            ]
        , fCol [ gap "20px" ]
            (List.map viewTaskItem b.items)
        ]


viewTaskItem : Task -> Html msg
viewTaskItem t =
    div
        [ bgc black
        , pa "10px"
        , style "border-radius" "5px"
        , style "border-left" "5px solid green"
        ]
        [ text t.title ]
