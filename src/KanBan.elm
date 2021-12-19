module KanBan exposing (main)

import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


type Msg
    = Msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Document Msg
view _ =
    Document "Kanban"
        [ basicStylesNode
        , stylesNode """
            html,body{

            }
        """
        , div
            [ pa "20px"
            , gap "20px"
            , dGrid
            , style "grid-auto-flow" "column"
            , bgc (grayN 0.18)
            ]
            (buckets
                |> List.map viewBucketColumn
            )
        ]


tasks =
    let
        toBucketId i =
            List.drop (modBy 3 i) emptyBuckets
                |> List.head
                |> Maybe.withDefault defaultBucket
                |> .id
    in
    times 10
        (\i ->
            Task (TaskId (fromInt i)) ("Demo Task #" ++ fromInt i) (toBucketId i)
        )


buckets =
    emptyBuckets
        |> List.indexedMap
            (\bi b ->
                { b
                    | items =
                        rangeStartSize ((bi * 4) + 1) 4
                            |> List.map
                                (\ti ->
                                    Task (TaskId (fromInt ti)) ("Demo Task #" ++ fromInt ti) b.id
                                )
                            |> List.take (bi + 2)
                }
            )


rangeStartSize s sz =
    List.range s (s + sz)


type TaskId
    = TaskId String


type alias Task =
    { id : TaskId
    , title : String
    , bucketId : BucketId
    }


type BucketId
    = BucketId String


type alias Bucket =
    { id : BucketId
    , title : String
    , color : String
    , items : List Task
    }


defaultBucket =
    Bucket (BucketId "Todo") "Todo" (hsl 0 0.7 0.5) []


emptyBuckets : List Bucket
emptyBuckets =
    [ defaultBucket
    , Bucket (BucketId "Ongoing") "Ongoing" (hsl 0.14 0.7 0.5) []
    , Bucket (BucketId "Done") "Done" (hsl 0.32 0.7 0.5) []
    ]


viewBucketColumn : Bucket -> Html msg
viewBucketColumn b =
    fCol [ gap "20px" ]
        [ div []
            [ div [ fontSize "22px", ttu, bold ] [ text b.title ]
            , div [ fontSize "18px", fg (grayN 0.6) ]
                [ text <| fromInt (List.length b.items) ++ " items" ]
            ]
        , fCol [ gap "20px" ]
            (List.map (viewTaskItem b) b.items)
        ]


viewTaskItem : Bucket -> Task -> Html msg
viewTaskItem b t =
    div
        [ bgc (grayN 0.13)
        , pa "20px"
        , style "border-radius" "10px"
        , style "border-left" ("10px solid " ++ b.color)
        , style "box-shadow" ("1px 2px 0px 1px " ++ hsla 0 0 0 1)
        ]
        [ text t.title ]
