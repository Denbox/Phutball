module V3 exposing (main)
import Browser
import Browser.Events exposing (onMouseDown, onMouseUp, onMouseMove)
import Html exposing (Html)
import Json.Decode exposing (..)
import Draw exposing (draw)
import Game exposing (..)

type Model =
  Playing Game Mouse

type Mouse
  = Pressing Point Point
  | Up
  -- = DraggingBall Point
  -- | Pressing Coord
  -- | Up

ballSelected : Game -> Point -> Bool
ballSelected game press_location =
  game.ball.coord == snap press_location

type Msg
  = MouseUp Point
  | MouseDown Point
  | MouseMove Point

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Playing game mouse ->
      case msg of
        MouseUp end ->
          case mouse of
            Pressing start current ->
              if ballSelected game start then
                if validBallTurnFinish game (snap end) then
                  (Playing (finishBallTurn game) Up, Cmd.none)
                else
                  (Playing (moveBall game (snap end)) Up, Cmd.none)
              else
                if snap end == snap start then
                  (Playing (placeStone game (snap end)) Up, Cmd.none)
                else
                  (model, Cmd.none)
            Up -> -- this case should never occur
              (model, Cmd.none)
        MouseDown point ->
          case mouse of
            Pressing _ _ -> -- this case should never occur
              (model, Cmd.none)
            Up ->
              (Playing game (Pressing point point), Cmd.none)
        MouseMove current ->
          case mouse of
            Pressing start _ ->
              (Playing game (Pressing start current), Cmd.none)
            _ ->
              (model, Cmd.none)

init : () -> (Model, Cmd Msg)
init _ =
  (Playing initialize Up, Cmd.none)

view : Model -> Html Msg
view model =
  case model of
    Playing game mouse ->
      case mouse of
        Pressing start current ->
          if ballSelected game start then
            draw game (Just current)
          else
            draw game Nothing
        Up ->
          draw game Nothing

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ Browser.Events.onMouseDown (Json.Decode.map MouseDown decodeXY)
  , Browser.Events.onMouseMove (Json.Decode.map MouseMove decodeXY)
  , Browser.Events.onMouseUp   (Json.Decode.map MouseUp   decodeXY)
  ]

decodeX  = (Json.Decode.field "pageX" Json.Decode.int)
decodeY  = (Json.Decode.field "pageY" Json.Decode.int)
decodeXY = (Json.Decode.map2 Point decodeX decodeY)

main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
