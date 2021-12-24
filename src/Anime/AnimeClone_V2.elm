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
view _ =
    Document "Anime V2"
        [ basicStylesNode
        , div []
            (initialParticles
                --|> applyAnimations particleAnimations 0 model.animClock
                |> List.map viewParticle
            )
        ]


type alias Anim =
    { from : Float
    , to : Float
    , start : Int
    , duration : Int
    , delay : Int
    , direction : Direction
    , loop : Loop
    }


startAnim : Int -> Anim
startAnim start =
    { from = 0
    , to = 1
    , start = start
    , duration = 1800
    , delay = 0
    , direction = Normal
    , loop = Times 1
    }


type Direction
    = Normal
    | Reverse
    | Alternate


type Loop
    = Infinite
    | Times Int


type alias Particle =
    { x : Float
    }


initialParticles : List Particle
initialParticles =
    let
        p =
            { x = 0 }
    in
    times 10 (\_ -> p)


viewParticle : Particle -> Html msg
viewParticle p =
    div
        [ style "transform" ("translateX(" ++ fromFloat p.x ++ "px)")
        , bgc <| hsl 0.2 1 0.5
        , borderRadius "10px"
        , styleWidth "50px"
        , styleHeight "50px"
        , ma "10px"
        ]
        []
