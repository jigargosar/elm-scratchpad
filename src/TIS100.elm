module TIS100 exposing (main)

import Browser.Dom
import Html
import TIS100.Addr exposing (Addr)
import TIS100.Effect as Eff exposing (Effect(..), withEff, withoutEff)
import TIS100.Puzzle as Puzzle
import TIS100.PuzzlePage as PuzzlePage
import TIS100.Saves as Saves exposing (Saves)
import Task
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


main =
    let
        init_ flags =
            init flags |> runEffect

        update_ msg model =
            update msg model |> runEffect
    in
    browserDocument
        { init = init_
        , subscriptions = subscriptions
        , update = update_
        , view = viewDocument
        }


type Page
    = PuzzlePage PuzzlePage.Model
    | SegmentListPage


type alias Model =
    { page : Page
    , saves : Saves
    }


init : () -> ( Model, Effect )
init () =
    { page = SegmentListPage
    , saves = Saves.initial
    }
        |> withEff Eff.autoFocus


type Msg
    = PuzzlePageMsg PuzzlePage.Msg
    | GotoPuzzle Puzzle.Id
    | OnFocus (Result Browser.Dom.Error ())


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PuzzlePage ppm ->
            PuzzlePage.subscriptions ppm
                |> Sub.map PuzzlePageMsg

        SegmentListPage ->
            Sub.none


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        PuzzlePageMsg sm ->
            case model.page of
                PuzzlePage ppm ->
                    let
                        ( page, effect ) =
                            PuzzlePage.update sm ppm
                    in
                    { model | page = PuzzlePage page }
                        |> withEff effect

                SegmentListPage ->
                    model |> withoutEff

        GotoPuzzle id ->
            let
                page =
                    PuzzlePage.init id (Saves.get id model.saves)
            in
            { model | page = PuzzlePage page } |> withoutEff

        OnFocus (Ok ()) ->
            model |> withoutEff

        OnFocus (Err err) ->
            let
                _ =
                    Debug.log "Focus Error: " err
            in
            model |> withoutEff


runEffect : ( Model, Effect ) -> ( Model, Cmd Msg )
runEffect ( model, effect ) =
    case effect of
        AutoFocus ->
            ( model, autoFocusCmd )

        None ->
            ( model, Cmd.none )

        ReturnToSegmentList ->
            ( { model | page = SegmentListPage }, autoFocusCmd )

        SavePuzzleSrc id puzzleSrc ->
            ( { model | saves = Saves.set id puzzleSrc model.saves }
            , Cmd.none
            )


autoFocusCmd : Cmd Msg
autoFocusCmd =
    focusCmd Eff.autoFocusId


focusCmd : String -> Cmd Msg
focusCmd hid =
    Browser.Dom.focus hid |> Task.attempt OnFocus


viewDocument : Model -> Document Msg
viewDocument model =
    Document "ELM TIS 100 CLONE"
        [ basicStylesNode

        --, node "SCRIPT" [ attribute "src" "https://livejs.com/live.js" ] []
        , case model.page of
            PuzzlePage page ->
                PuzzlePage.view page |> Html.map PuzzlePageMsg

            SegmentListPage ->
                div [ displayGrid, placeContentCenter ]
                    (Puzzle.allIds
                        |> List.map segmentBtn
                    )
        ]


segmentBtn : Puzzle.Id -> Html Msg
segmentBtn id =
    button
        [ Eff.attrAutoFocusId
        , notifyClick (GotoPuzzle id)
        ]
        [ text (Puzzle.titleFromId id) ]
