module KanBan exposing (main)

import Browser.Events
import Dict exposing (Dict)
import Html exposing (span)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
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
    { taskDict : TaskDict
    , input : String
    , drag : Drag
    }


draggedTaskId : Model -> Maybe TaskId
draggedTaskId model =
    case model.drag of
        DraggingTag { dragged } ->
            Just dragged.id

        _ ->
            Nothing


draggedTaskDetails : Model -> Maybe ( ( Dragging, Float2 ), ( Bucket, Task ) )
draggedTaskDetails model =
    case model.drag of
        DraggingTag dragging ->
            let
                (TaskId id) =
                    dragging.dragged.id
            in
            Dict.get id model.taskDict
                |> Maybe.andThen
                    (\t ->
                        initialBuckets
                            |> findFirst (propEq .id t.bucketId)
                            |> Maybe.map (pairTo t >> pair ( dragging, ( 263, 66 ) ))
                    )

        _ ->
            Nothing


type Drag
    = NotDragging
    | DraggingTag Dragging


type alias Dragging =
    { pageXY : Float2
    , clientXY : Float2
    , dragged : { id : TaskId, size : Float2 }
    , draggedOver : Maybe TaskId
    }


type alias TaskDict =
    Dict String Task


sortedTasksInBucketWithId : BucketId -> TaskDict -> List Task
sortedTasksInBucketWithId bucketId taskDict =
    Dict.values taskDict
        |> List.filter (propEq .bucketId bucketId)
        |> List.sortBy .sortOrder


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
                    { id = TaskId (fromInt i)
                    , title = "Demo Task #" ++ fromInt i
                    , bucketId = toBucketId i
                    , sortOrder = -i
                    }
                )
    in
    ( { taskDict = demoTasks |> dictBy (.id >> (\(TaskId id) -> id))
      , input = ""
      , drag =
            NotDragging
                |> always
                    ((demoTasks
                        |> List.head
                        |> Maybe.map
                            (\t ->
                                DraggingTag
                                    { pageXY = ( 300, 500 )
                                    , clientXY = ( 300, 500 )
                                    , dragged = { id = t.id, size = ( 263, 66 ) }
                                    , draggedOver = Nothing
                                    }
                            )
                     )
                        |> Maybe.withDefault NotDragging
                    )
      }
    , Cmd.none
    )


type Msg
    = NOP
    | InputChanged String
    | OnInputSubmit
    | CreateTask String TaskId
    | OnMouseMove MouseEvent
    | MouseMovedOverTask TaskId MouseEvent


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onMouseMove (JD.map OnMouseMove mouseEventDecoder)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        InputChanged input ->
            ( { model | input = input }, Cmd.none )

        OnInputSubmit ->
            ( { model | input = "" }, createTaskCmd model.input )

        CreateTask title taskId ->
            ( { model | taskDict = createTask title taskId model.taskDict }
            , Cmd.none
            )

        OnMouseMove mouseEvent ->
            ( case model.drag of
                DraggingTag dragging ->
                    { model
                        | drag =
                            DraggingTag
                                { dragging
                                    | pageXY = mouseEvent.pageXY
                                    , clientXY = mouseEvent.clientXY
                                }
                    }

                _ ->
                    model
            , Cmd.none
            )

        MouseMovedOverTask taskId mouseEvent ->
            ( case model.drag of
                NotDragging ->
                    model

                DraggingTag dragging ->
                    { model
                        | drag =
                            DraggingTag
                                { dragging
                                    | draggedOver = Just taskId
                                }
                    }
            , Cmd.none
            )


createTask : String -> TaskId -> TaskDict -> TaskDict
createTask title taskId taskDict =
    insertBy (.id >> (\(TaskId id) -> id))
        (Task taskId title defaultBucket.id maxInt)
        taskDict
        |> normalizeTaskSortOrder


normalizeTaskSortOrder : TaskDict -> TaskDict
normalizeTaskSortOrder taskDict =
    taskDict
        |> Dict.values
        |> groupBy .bucketId
        |> List.concatMap
            (\( h, t ) ->
                (h :: t)
                    |> List.sortBy .sortOrder
                    |> List.indexedMap setSortOrder
            )
        |> dictBy (.id >> (\(TaskId id) -> id))


setSortOrder i m =
    { m | sortOrder = i }


createTaskCmd : String -> Cmd Msg
createTaskCmd title =
    Random.generate (CreateTask title) randomTaskId


view : Model -> Document Msg
view model =
    Document "Kanban"
        [ basicStylesNode
        , stylesNode """
            input{
                font-size: inherit;
                padding: 0.5rem;
            }
        """
        , div [ bgc (grayN 0.18) ]
            [ fCol []
                [ Html.input
                    [ HA.placeholder "What's on your mind?"
                    , autofocus True
                    , onInput InputChanged
                    , onEnter OnInputSubmit
                    , HA.value model.input
                    ]
                    []
                ]
            , div
                [ pa "20px"
                , gap "20px"
                , dGrid
                , style "grid-auto-flow" "column"
                , positionRelative
                ]
                (initialBuckets
                    |> List.map
                        (\b ->
                            viewBucketColumn (draggedTaskId model)
                                b
                                (sortedTasksInBucketWithId b.id model.taskDict)
                        )
                )
            , draggedTaskDetails model
                |> Maybe.map viewDraggedTaskItem
                |> viewMaybe
            , case model.drag of
                NotDragging ->
                    noView

                DraggingTag _ ->
                    stylesNode """
                    * {
                      cursor: grabbing!important;
                      user-select:none!important;
                    }
                    """
            ]
        ]


viewMaybe =
    Maybe.withDefault noView


noView =
    text ""


type TaskId
    = TaskId String


randomTaskId =
    randomId |> Random.map TaskId


type alias Task =
    { id : TaskId
    , title : String
    , bucketId : BucketId
    , sortOrder : Int
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


viewBucketColumn : Maybe TaskId -> Bucket -> List Task -> Html Msg
viewBucketColumn mbDraggedTaskId b tasks =
    fCol [ gap "20px" ]
        [ div []
            [ div [ fontSize "22px", ttu, bold ] [ text b.title ]
            , div [ fontSize "18px", fg (grayN 0.6) ]
                [ text <| fromInt (List.length tasks) ++ " items" ]
            ]
        , fCol [ gap "20px" ]
            (List.map (viewTaskItem mbDraggedTaskId b) tasks)
        ]


viewDraggedTaskItem : ( ( Dragging, Float2 ), ( Bucket, Task ) ) -> Html Msg
viewDraggedTaskItem ( ( md, ( w, h ) ), ( b, t ) ) =
    let
        ( x, y ) =
            md.clientXY
    in
    div
        ([ bgc (grayN 0.13)
         , pa "20px"
         , style "border-radius" "10px"
         , style "border-left" ("10px solid " ++ b.color)
         , style "box-shadow" ("1px 2px 0px 1px " ++ hsla 0 0 0 1)
         , HA.draggable "true"
         , cursorGrab
         ]
            ++ [ HA.draggable "false"
               , noPointerEvents
               , style "opacity" "0.9"
               , positionFixed
               , style "left" <| fpx (x - (w / 2))
               , style "top" <| fpx (y - (h / 2))
               , styleWidthFPx w
               , styleHeightFPx h
               ]
        )
        [ span
            ([ userSelectText, cursorText ]
                ++ [ cursorInherit
                   ]
            )
            [ text t.title ]
        ]


viewTaskItem : Maybe TaskId -> Bucket -> Task -> Html Msg
viewTaskItem mbDraggedTaskId b t =
    div
        ([ bgc (grayN 0.13)
         , pa "20px"
         , style "border-radius" "10px"
         , style "border-left" ("10px solid " ++ b.color)
         , style "box-shadow" ("1px 2px 0px 1px " ++ hsla 0 0 0 1)
         , HA.draggable "true"
         , cursorGrab
         ]
            ++ (case mbDraggedTaskId of
                    Just id ->
                        [ HA.draggable "false"
                        ]
                            ++ (if id == t.id then
                                    [ style "opacity" "0.3"
                                    ]

                                else
                                    [ HE.on "mousemove"
                                        (JD.map (MouseMovedOverTask t.id)
                                            mouseEventDecoder
                                            |> JD.map (Debug.log "mousemove")
                                        )
                                    ]
                               )

                    Nothing ->
                        []
               )
        )
        [ span [ userSelectText, cursorText ]
            [ text t.title ]
        ]
