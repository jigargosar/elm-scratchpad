module AnimeClone exposing (main)

import Utils exposing (..)


type alias Particle =
    { charge : String
    , cycles : Int
    }


initialParticle : Particle
initialParticle =
    { charge = "80%", cycles = 120 }


anime =
    animate 0
        0
        { from = .obj >> .cycles
        , to = always 130
        , duration = always 1800
        , delay = always 0
        , setter = \v o -> { o | cycles = v }
        , interpolator = lerpInt
        }
        initialParticle


animate : Int -> Int -> AnimConfig o v -> o -> o
animate start now config obj =
    Debug.todo "todo"


type alias Args o =
    { obj : o
    , index : Int
    , length : Int
    }


type alias AnimConfig o v =
    { from : Args o -> v
    , to : Args o -> v
    , duration : Args o -> Int
    , delay : Args o -> Int
    , setter : v -> o -> o
    , interpolator : v -> v -> Float -> v
    }


main =
    div [] []
