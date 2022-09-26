module TIS100 exposing (main)

import Browser.Dom
import Html
import TIS100.Addr exposing (Addr)
import TIS100.Effect as Eff exposing (Effect(..), withEff, withoutEff)
import TIS100.Puzzle as Puzzle
import TIS100.PuzzlePage as PuzzlePage
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
    , saves : List ( Addr, String )
    }


init : () -> ( Model, Effect )
init () =
    { page = SegmentListPage
    , saves = signalComparatorSourceEntries
    }
        |> withEff Eff.autoFocus


signalComparatorSourceEntries : List ( Addr, String )
signalComparatorSourceEntries =
    [ ( ( 0, 1 ), "MOV UP DOWN" )
    , ( ( 0, 2 ), "MOV UP DOWN" )
    , ( ( 0, 3 ), "MOV UP right" )
    , ( ( 1, 1 ), "" )
    , ( ( 1, 2 ), "" )
    , ( ( 1, 3 )
      , [ "S: MOV LEFT ACC"
        , "MOV ACC RIGHT"
        , ""
        , "JGZ 1"
        , "MOV 0 DOWN"
        , "JMP S"
        , ""
        , "1: MOV 1 DOWN"
        ]
            |> String.join "\n"
      )
    , ( ( 2, 1 ), "" )
    , ( ( 2, 2 ), "" )
    , ( ( 2, 3 )
      , [ "S: MOV LEFT ACC"
        , "MOV ACC RIGHT"
        , ""
        , "JEZ 1"
        , "MOV 0 DOWN"
        , "JMP S"
        , ""
        , "1: MOV 1 DOWN"
        ]
            |> String.join "\n"
      )
    , ( ( 3, 1 ), "" )
    , ( ( 3, 2 ), "" )
    , ( ( 3, 3 )
      , [ "S: MOV LEFT ACC"
        , "# MOV ACC RIGHT"
        , ""
        , "JLZ 1"
        , "MOV 0 DOWN"
        , "JMP S"
        , ""
        , "1: MOV 1 DOWN"
        ]
            |> String.join "\n"
      )
    ]


initPuzzlePage saves =
    PuzzlePage.init
        Puzzle.signalComparator
        saves


type Msg
    = PuzzlePageMsg PuzzlePage.Msg
    | GotoPuzzle
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

        GotoPuzzle ->
            { model | page = PuzzlePage (initPuzzlePage model.saves) }
                |> withoutEff

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
                    [ button
                        [ Eff.attrAutoFocusId
                        , notifyClick GotoPuzzle
                        ]
                        [ text "go to puzzle" ]
                    ]
        ]
