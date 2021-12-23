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
    animateProperty
        { from = .obj >> .cycles
        , to = always 130
        , duration = always 1800
        , delay = always 0
        , setter = \v o -> { o | cycles = v }
        }


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
    }


type alias System o v =
    {}


animateProperty : AnimConfig o v -> System o v
animateProperty config =
    Debug.todo "return animation system"


main =
    div [] []
