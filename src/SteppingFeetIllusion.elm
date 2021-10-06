module SteppingFeetIllusion exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import TypedSvg.Attributes as TA
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
            [ Brick 0 ((height / 3) - brickHeight / 2) "white" 1
            , Brick 0 ((height * 2 / 3) - brickHeight / 2) "black" 1
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
        , style "background-color" "#333"
        , style "background-color" "#0f0f0f"
        ]
        [ model.bricks
            |> List.map viewBrick
            |> Svg.g []
        , createConnections model.bricks []
            |> List.map viewSegment
            |> Svg.g []
        ]


type alias Point =
    ( Float, Float )


type alias Seg =
    ( Point, Point )


createConnections : List Brick -> List Seg -> List Seg
createConnections pendingParticles connections =
    case pendingParticles of
        [] ->
            connections

        p :: ps ->
            createConnections ps (createConnectionsHelp p ps ++ connections)


createConnectionsHelp : Brick -> List Brick -> List Seg
createConnectionsHelp a =
    let
        pointA =
            ( a.x, a.y )

        maxDistance =
            85
    in
    List.filterMap
        (\b ->
            let
                pointB =
                    ( b.x, b.y )

                dist =
                    ((b.x - a.x) ^ 2)
                        + ((b.y - a.y) ^ 2)
                        |> sqrt
            in
            if dist < maxDistance then
                Just ( pointA, pointB )

            else
                Nothing
        )


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


viewSegment : Seg -> Svg msg
viewSegment ( a, b ) =
    Svg.polyline
        [ TA.points [ a, b ]
        , SA.stroke "rgba(255,255,255,0.05)"
        ]
        []
