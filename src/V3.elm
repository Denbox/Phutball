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
              let
                release_mouse : Result String Game -> (Model, Cmd Msg)
                release_mouse result_game =
                  (Playing (Result.withDefault game result_game) Up, Cmd.none)
              in
                if ballSelected game start then
                  if validBallTurnFinish game (snap end) then
                    release_mouse (finishBallTurn game (snap end))
                  else
                    release_mouse (moveBall game (snap end))
                else
                  if snap end == snap start then
                    release_mouse (placeStone game (snap end))
                  else
                    release_mouse (Ok game)
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
