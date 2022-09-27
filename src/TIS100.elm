port module TIS100 exposing (main)

import Browser.Dom
import Html
import Json.Decode as JD
import Json.Encode exposing (Value)
import TIS100.Effect as Eff exposing (Effect(..), withEff, withoutEff)
import TIS100.Puzzle as Puzzle
import TIS100.PuzzlePage as PuzzlePage
import TIS100.Saves as Saves exposing (Saves)
import TIS100.UI as UI
import Task
import Utils exposing (..)



{-
   THIS LINE IS FOR FIXING INDENTATION ISSUE WITH ELM-FORMAT. DETAILS?
    # Specs:


-}


port toJSSave : Value -> Cmd msg


type alias Flags =
    { saves : Value }


main : Program Flags Model Msg
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


init : Flags -> ( Model, Effect )
init flags =
    let
        saves =
            case JD.decodeValue Saves.decoder flags.saves of
                Err e ->
                    Debug.log "Debug: " e
                        |> Debug.todo "err"

                Ok s ->
                    s
    in
    { page = SegmentListPage
    , saves = saves
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
            let
                saves =
                    Saves.set id puzzleSrc model.saves
            in
            ( { model | saves = saves }
            , toJSSave (Saves.encode saves)
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
        , UI.commonStyles
        , case model.page of
            PuzzlePage page ->
                PuzzlePage.view page |> Html.map PuzzlePageMsg

            SegmentListPage ->
                viewSegmentListPage
        ]


viewSegmentListPage : Html Msg
viewSegmentListPage =
    div
        [ displayGrid
        , pa "2ch"
        , gap "1ch"
        , gridTemplateColumns "repeat(5, 1fr)"
        , gridTemplateRows "repeat(5, 1fr)"
        , sMaxHeight "100vh"
        ]
        (Puzzle.allIds
            |> listMapHeadAndRest viewSegmentAutoFocus viewSegment
        )


listMapHeadAndRest : (a -> b) -> (a -> b) -> List a -> List b
listMapHeadAndRest fh fr ls =
    case ls of
        [] ->
            []

        h :: r ->
            fh h :: List.map fr r


viewSegmentAutoFocus : Puzzle.Id -> Html Msg
viewSegmentAutoFocus =
    viewSegmentHelp [ Eff.attrAutoFocusId ]


viewSegment : Puzzle.Id -> Html Msg
viewSegment =
    viewSegmentHelp []


viewSegmentHelp : List (Attribute Msg) -> Puzzle.Id -> Html Msg
viewSegmentHelp attrs id =
    UI.btn (pa "0.5ch" :: attrs)
        (Just (GotoPuzzle id))
        (Puzzle.titleFromId id)
