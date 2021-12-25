module Anime.AnimeClone_V2 exposing (main)

import Anime.Anim as A
import Browser.Events
import Ease
import Json.Decode as JD
import Utils exposing (..)


main =
    bDocument
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { animClock : A.AnimClock
    , particles : List Particle
    }


particlesForRendering : Model -> List Particle
particlesForRendering model =
    model.particles
        |> List.map (updateParticleAnim model.animClock)


initParticle : IndexLength -> Particle
initParticle il =
    let
        defaultAttrs =
            [ A.setDuration 1800
            , A.loopTimes 3
            , A.alternateDirection
            , A.setEasing Ease.outBounce
            , A.setDelay <| round <| (A.staggerFromCenter 200 il + 500)
            ]
    in
    { x = 0
    , xa =
        A.anim
            ([ defaultAttrs, [ A.fromTo 0 270 ] ] |> List.concat)
    , a = 0
    , aa =
        A.anim
            (defaultAttrs
                ++ [ A.setTo <| A.staggerRange ( -360, 360 ) il
                   , A.setTo 0
                   ]
            )
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        initialParticles : List Particle
        initialParticles =
            timesWithIndexAndLength 6 initParticle
    in
    ( { animClock = A.animClockInit
      , particles = initialParticles
      }
    , Cmd.none
    )


type Msg
    = NOP
    | OnAnimClockDelta A.AnimClockDelta
    | OnClick


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ A.animClockSubscription OnAnimClockDelta
    , Browser.Events.onClick (JD.succeed OnClick)
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NOP ->
            ( model, Cmd.none )

        OnAnimClockDelta delta ->
            ( { model
                | animClock = A.animClockUpdateOnDelta delta model.animClock
              }
            , Cmd.none
            )

        OnClick ->
            ( { model | animClock = A.animClockInit }, Cmd.none )


view : Model -> Document Msg
view model =
    Document "Anime V2"
        [ basicStylesNode
        , div []
            (particlesForRendering model
                |> List.map viewParticle
            )
        ]


type alias Particle =
    { x : Float
    , xa : A.Anim
    , a : Float
    , aa : A.Anim
    }


updateParticleAnim : A.AnimClock -> Particle -> Particle
updateParticleAnim ac p =
    { p
        | x = A.valueAt p.xa ac
        , a = A.valueAt p.aa ac
    }


viewParticle : Particle -> Html msg
viewParticle p =
    div
        [ style "transform"
            ([ "translateX(" ++ fromFloat p.x ++ "px)"
             , "rotate(" ++ fromFloat p.a ++ "deg)"
             ]
                |> String.join " "
            )
        , bgc <| hsl 0.2 1 0.5
        , fg black
        , borderRadius "10px"
        , styleWidth "50px"
        , styleHeight "50px"
        , ma "10px"
        , dGrid
        , placeContentCenter
        ]
        [ div [ fontSize "25px" ] [ text "A" ] ]
