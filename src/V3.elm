module V3 exposing (..)
import Browser
import Browser.Events exposing (onMouseDown, onMouseUp, onMouseMove)
import Html exposing (Html)
import Json.Decode exposing (..)
import Draw exposing (draw)
import Game exposing (..)

type Model =
  Playing Game Mouse

type Mouse
  = DraggingBall Point
  | Pressing Coord
  | Up

type Msg
  = MouseUp Point
  | MouseDown Point
  | MouseMove Point

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Playing game mouse ->
      case msg of
        MouseUp point ->
          case mouse of
            DraggingBall _ ->
              -- try to do a ball move
              if validBallTurnFinish game (snap point) then
                (Playing (finishBallTurn game) Up, Cmd.none)
              else
                (Playing (moveBall game (snap point)) Up, Cmd.none)
            Pressing start_coord ->
              -- try to place stone
              if snap point == start_coord then
                (Playing (placeStone game (snap point)) Up, Cmd.none)
              else
                (model, Cmd.none)
            Up -> -- this case should never occur
              (model, Cmd.none)
        MouseDown point ->
          case mouse of
            DraggingBall _ -> -- this case should never occur
              (model, Cmd.none)
            Pressing _ -> -- this case should never occur
              (model, Cmd.none)
            Up ->
              if snap point == game.ball.coord then
                (Playing game (DraggingBall point), Cmd.none)
              else
                (Playing game (Pressing (snap point)), Cmd.none)
        MouseMove point ->
          case mouse of
            DraggingBall _ ->
              (Playing game (DraggingBall point), Cmd.none)
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
        DraggingBall point ->
          draw game (Just point)
        _ ->
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
