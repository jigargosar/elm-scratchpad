module SteppingFeetIllusion exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes.InPx as Px


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Brick =
    { x : Float
    , y : Float
    , color : String
    , vx : Float
    }


moveByVelocity : Brick -> Brick
moveByVelocity ({ x, y, vx } as b) =
    { b | x = x + vx }


bounceAgainstScreenEdge : Brick -> Brick
bounceAgainstScreenEdge ({ x, vx } as b) =
    { b
        | x = clamp 0 (width - brickWidth) x
        , vx =
            if x < 0 || x > (width - brickWidth) then
                negate vx

            else
                vx
    }


type alias Model =
    { bricks : List Brick }


init : () -> ( Model, Cmd Msg )
init () =
    ( { bricks =
            [ { x = 0
              , y = (height / 3) - brickHeight / 2
              , vx = 1
              , color = "white"
              }
            , { x = 0
              , y = (height * 2 / 3) - brickHeight / 2
              , vx = 1
              , color = "black"
              }
            ]
      }
    , Cmd.none
    )


type Msg
    = OnTick


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / 60) (\_ -> OnTick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTick ->
            ( { model
                | bricks =
                    model.bricks
                        |> List.map (moveByVelocity >> bounceAgainstScreenEdge)
              }
            , Cmd.none
            )


width =
    500


brickWidth =
    width / 8


barWidth =
    brickWidth / 8


height =
    500


brickHeight =
    brickWidth / 2


view : Model -> Html Msg
view model =
    Svg.svg
        [ SA.width (String.fromFloat width)
        , SA.height (String.fromFloat height)
        , SA.fill "none"
        , SA.stroke "none"
        , style "background-color" "black"
        ]
        [ List.range 0 (ceiling (width / barWidth))
            |> List.filter isEven
            |> List.map viewBar
            |> Svg.g []
        , model.bricks
            |> List.map viewBrick
            |> Svg.g []
        ]


isEven i =
    modBy 2 i == 0


viewBar i =
    let
        x =
            toFloat i * barWidth
    in
    Svg.rect
        [ Px.x x
        , Px.y 0
        , Px.width barWidth
        , Px.height height
        , SA.fill "white"
        ]
        []


viewBrick : Brick -> Svg msg
viewBrick { x, y, color } =
    Svg.rect
        [ Px.x x
        , Px.y y
        , Px.width brickWidth
        , Px.height brickHeight
        , SA.fill color
        ]
        []
