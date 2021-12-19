module KanBan exposing (main)

import Dict exposing (Dict)
import Random
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { taskDict : TaskDict }


type alias TaskDict =
    Dict String Task


init : () -> ( Model, Cmd Msg )
init () =
    let
        demoTasks : List Task
        demoTasks =
            let
                toBucketId i =
                    List.drop (modBy 3 i) initialBuckets
                        |> List.head
                        |> Maybe.withDefault defaultBucket
                        |> .id
            in
            times 10
                (\i ->
                    Task (TaskId (fromInt i)) ("Demo Task #" ++ fromInt i) (toBucketId i)
                )
    in
    ( { taskDict = demoTasks |> dictBy (.id >> (\(TaskId id) -> id))
      }
    , Cmd.none
    )


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
view model =
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
            (model.taskDict
                |> Dict.values
                |> groupEqBy .bucketId
                |> List.sortBy (first >> .bucketId >> sortOrderOfBucketWithId >> Maybe.withDefault Random.maxInt)
                |> List.filterMap
                    (\( h, t ) ->
                        bucketWithId h.bucketId
                            |> Maybe.map
                                (\b ->
                                    viewBucketColumn b (h :: t)
                                )
                    )
            )
        ]


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
    , sortOrder : Int
    , title : String
    , color : String
    }


defaultBucket =
    Bucket (BucketId "Todo") 0 "Todo" (hsl 0 0.7 0.5)


initialBuckets : List Bucket
initialBuckets =
    [ defaultBucket
    , Bucket (BucketId "Ongoing") 1 "Ongoing" (hsl 0.14 0.7 0.5)
    , Bucket (BucketId "Done") 2 "Done" (hsl 0.32 0.7 0.5)
    ]


sortOrderOfBucketWithId : BucketId -> Maybe Int
sortOrderOfBucketWithId =
    bucketWithId >> Maybe.map .sortOrder


bucketWithId : BucketId -> Maybe Bucket
bucketWithId (BucketId bucketId) =
    let
        bucketDict =
            List.foldl (insertBy (.id >> (\(BucketId id) -> id))) Dict.empty initialBuckets
    in
    Dict.get bucketId bucketDict


viewBucketColumn : Bucket -> List Task -> Html msg
viewBucketColumn b tasks =
    fCol [ gap "20px" ]
        [ div []
            [ div [ fontSize "22px", ttu, bold ] [ text b.title ]
            , div [ fontSize "18px", fg (grayN 0.6) ]
                [ text <| fromInt (List.length tasks) ++ " items" ]
            ]
        , fCol [ gap "20px" ]
            (List.map (viewTaskItem b) tasks)
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
