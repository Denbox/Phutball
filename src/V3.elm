module V3 exposing (..)
import Browser
import Browser.Events exposing (onMouseDown, onMouseUp, onMouseMove)
import Html exposing (Html)
import Json.Decode exposing (..)
import Draw exposing (..)
import Game exposing (..)

type Model =
  Playing Game

type Msg
  = MouseUp Point
  | MouseDown Point

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

init : () -> (Model, Cmd Msg)
init _ =
  (Playing <| Game (Ball (Coord 7 9) [] Nothing) [Coord 5 2, Coord 10 10] X, Cmd.none)

view : Model -> Html Msg
view model =
  case model of
    Playing game ->
      let
        stones = game.stones
        ball_position = Game.getBallPoint game.ball
        ball_history = ball_position::(List.map Game.unSnap game.ball.history)
        dragging = game.ball.point /= Nothing
      in
        Draw.draw game

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing game ->
      -- case mouse of
        -- Up ->
          Sub.batch
          [ Browser.Events.onMouseDown (Json.Decode.map MouseDown decodeGridXY)
          , Browser.Events.onMouseUp   (Json.Decode.map MouseUp   decodeGridXY)
          ]
        -- _ ->
        --   Sub.batch
        --   [ Browser.Events.onMouseMove (Json.Decode.map MouseMove decodeXY)
        --   , Browser.Events.onMouseUp   (Json.Decode.map MouseUp   decodeGridXY)
        --   ]

decodeX  = (Json.Decode.field "pageX" Json.Decode.int)
decodeY  = (Json.Decode.field "pageY" Json.Decode.int)
decodeXY = (Json.Decode.map2 Point decodeX decodeY)
toGrid x y = Game.snap <| Point x y
decodeGridXY = (Json.Decode.map2 toGrid decodeX decodeY)

main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
