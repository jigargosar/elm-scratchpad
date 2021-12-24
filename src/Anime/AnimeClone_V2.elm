module Anime.AnimeClone_V2 exposing (main)

import Browser.Events
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { animClock : Int }


init : () -> ( Model, Cmd Msg )
init () =
    ( { animClock = 0 }, Cmd.none )


type Msg
    = NOP
    | OnAnimationFrameDeltaMilli Int


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (round >> OnAnimationFrameDeltaMilli)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnAnimationFrameDeltaMilli animationFrameDeltaMilli ->
            ( { model
                | animClock = model.animClock + clamp 0 100 animationFrameDeltaMilli
              }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    Document "Anime"
        [ basicStylesNode
        , div []
            (initialParticles
                --|> applyAnimations particleAnimations 0 model.animClock
                |> List.map viewParticle
            )
        ]


type alias Particle =
    { charge : String
    , cycles : Int
    , x : Float
    }


initialParticles : List Particle
initialParticles =
    let
        p =
            { charge = "0%", cycles = 120, x = 0 }
    in
    times 10 (\_ -> p)


viewParticle : Particle -> Html msg
viewParticle p =
    div
        [ style "transform" ("translateX(" ++ fromFloat p.x ++ "px)")
        , bgc <| hsl 0.2 1 0.5
        , fg transparent
        , styleWidth "50px"
        , styleHeight "50px"
        , ma "10px"
        ]
        [ div [] [ text p.charge ]
        , div [] [ text <| fromInt p.cycles ]
        ]
