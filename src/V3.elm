module V3 exposing (main)
import Browser
import Browser.Events exposing (onMouseDown, onMouseUp, onMouseMove)
import Html exposing (Html)
import Html.Events exposing (custom)
import Json.Decode exposing (Decoder, field, int, map, map2, succeed)
import Draw exposing (draw)
import Game exposing (..)

type Model =
  Playing Game Mouse

type Mouse
  -- = Pressing Side Point Point
  = LeftPress Point Point
  | RightPress Point Point
  | Up

type Side
  = Left
  | Right

ballSelected : Game -> Point -> Bool
ballSelected game press_location =
  game.ball.coord == snap press_location

type Msg
  = MouseUp Point
  | MouseDown Side Point
  | MouseMove Point

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Playing game mouse ->
      case msg of
        MouseUp end ->
          let
            release_mouse : Result String Game -> (Model, Cmd Msg)
            release_mouse result_game =
              (Playing (Result.withDefault game result_game) Up, Cmd.none)
          in
            case mouse of
              LeftPress start _ ->
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
              RightPress start _ ->
                release_mouse (undoBallMove game (snap end))
                -- (model, Cmd.none)
              Up -> -- this case should never occur
                (model, Cmd.none)
        MouseDown side point ->
          case mouse of
            LeftPress _ _ -> -- this case should never occur
              (model, Cmd.none)
            RightPress _ _ -> -- this case should never occur
              (model, Cmd.none)
            Up ->
              (Playing game (LeftPress point point), Cmd.none)
        MouseMove current ->
          case mouse of
            LeftPress start _ ->
              (Playing game (LeftPress start current), Cmd.none)
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
        LeftPress start current ->
          if ballSelected game start then
            draw game (Just current)
          else
            draw game Nothing
        _ ->
          draw game Nothing

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
  [ onMouseDown (map2 selectPressType decodeSide decodeXY)
  , onMouseMove (map MouseMove decodeXY)
  , onMouseUp   (map MouseUp   decodeXY)
  ]

decodeSide = (field "which" int)
decodeX       = (field "pageX" int)
decodeY       = (field "pageY" int)
decodeXY      = (map2 Point decodeX decodeY)

-- selectPressType : Int -> Point -> Sub Msg
selectPressType mouse_button_used point =
  if mouse_button_used == 3 then
    MouseDown Right point
  else
    MouseDown Left point

-- onRightClick msg =
--   custom
--     "contextmenu"
--     { stopPropogation = True
--     , preventDefault = True
--     }
--     (succeed msg)
onRightClick msg =
  custom
    "contextmenu"
    (succeed { message = msg, stopPropagation = True, preventDefault = True })

-- stopRightClickPropogation _ =
--   stopPropogationOn
--     "contextmenu"
--     (succeed True)


main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
