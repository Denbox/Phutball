module V3 exposing (..)
import Browser
import Browser.Events exposing (onMouseDown, onMouseUp, onMouseMove)
import Html exposing (Html)
import Json.Decode exposing (..)
import Draw exposing (draw)
import Game exposing (..)

type Model =
  Playing Game

type Msg
  = MouseUp Point
  | MouseDown Point
  | MouseMove Point

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Playing game ->
      case msg of
        MouseUp point ->
          if draggingBall game then
            if validBallTurnFinish game (snap point) then
              (Playing <| finishBallTurn game, Cmd.none)
            else if validBallMove game (snap point) then
              (Playing <| moveBall game (snap point), Cmd.none)
            else
              (model, Cmd.none)
          else
            if validPlacement game (snap point) then
              (Playing <| placeStone game (snap point), Cmd.none)
            else
              (model, Cmd.none)
        MouseDown point ->
          (Playing <| enterMousePress game point, Cmd.none)
        MouseMove point ->
          if draggingBall game then
            (Playing <| dragBall game point, Cmd.none)
          else
            (model, Cmd.none)
    -- _ ->
    --   (model, Cmd.none)

init : () -> (Model, Cmd Msg)
init _ =
  (Playing <| initialize, Cmd.none)

view : Model -> Html Msg
view model =
  case model of
    Playing game ->
      draw game

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing game ->
      Sub.batch
      [ Browser.Events.onMouseDown (Json.Decode.map MouseDown decodeXY)
      , Browser.Events.onMouseMove (Json.Decode.map MouseMove decodeXY)
      , Browser.Events.onMouseUp   (Json.Decode.map MouseUp   decodeXY)
      ]

decodeX  = (Json.Decode.field "pageX" Json.Decode.int)
decodeY  = (Json.Decode.field "pageY" Json.Decode.int)
decodeXY = (Json.Decode.map2 Point decodeX decodeY)
-- toGrid x y = Game.snap <| Point x y
-- decodeGridXY = (Json.Decode.map2 toGrid decodeX decodeY)

main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
