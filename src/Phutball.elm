module Phutball exposing (..)
import Browser
import Browser.Events exposing (onClick)
import Json.Decode exposing (..)
import Html exposing (Html, div)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

square_width = 35
margin = square_width
w = square_width * (15-1)
h = square_width * (19+2-1)

type alias Coords =
  { x : Int -- 0 to 20 inclusive (1 to 19 inclusive placeable)
  , y : Int -- 1 to 15 inclusive (all placeable)
  }

type alias Ball =
  { dragging : Bool
  , coords : Coords
  , drawLoc : Coords
  -- , history : List Coords
  }

type Player
 = X
 | O

type alias Game =
  { ball   : Ball
  , people : List Coords
  , turn   : Player
  }

type Model
  = Ready    Game
  | Playing  Game
  | Finished Game

type Msg
  = Click Coords
  | BeginDrag
  | Drag Coords
  | Drop Coords

opposite : Player -> Player
opposite p =
  case p of
    X -> O
    O -> X

placeable : Coords -> Game -> Bool
placeable coords game = (empty coords game) && (inPlaceableBounds coords)

empty : Coords -> Game -> Bool
empty coords game = not (List.member coords (game.ball.coords :: game.people))

inPlaceableBounds : Coords -> Bool
inPlaceableBounds coords = (List.member coords.x (List.range 0 14)) && (List.member coords.y (List.range 1 19))

inDraggableBounds : Coords -> Bool
inDraggableBounds coords = (List.member coords.x (List.range 0 14)) && (List.member coords.y (List.range 0 20))

validJump : Coords -> Game -> Bool
validJump coords game = allowedLine game.ball.coords coords && peopleInBetween coords game

diag : Coords -> Coords -> Bool
diag start end = (abs (start.x - end.x)) == (abs (start.y - end.y))

cross : Coords -> Coords -> Bool
cross start end = (start.x == end.x) || (start.y == end.y)

allowedLine : Coords -> Coords -> Bool
allowedLine start end = diag start end || cross start end

peopleInBetween : Coords -> Game -> Bool
peopleInBetween coords game =
  let
    p1 = coords
    p2 = game.ball.coords
    dx = if p1.x == p2.x then 0 else (p2.x-p1.x) // (abs p2.x-p1.x)
    dy = if p1.y == p2.y then 0 else (p2.y-p1.y) // (abs p2.y-p1.y)
    n = Basics.max (p2.x-p1.x) (p2.y-p1.y)
    between = List.map (\i -> Coords (p1.x+dx*i) (p1.y+dy*i)) (List.range 1 (n-1))
  in
    List.all (\x -> List.member x game.people) between

validDrag : Coords -> Game -> Bool
validDrag coords game = (empty coords game) && (inDraggableBounds coords) && (validJump coords game)


main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
  }

init : () -> (Model, Cmd Msg)
init _ = (Playing (Game (Ball False (Coords 7 9) (loc <| Coords 7 9)) [] X), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Playing game ->
      case msg of
        Click coords ->
          if placeable coords game then
            (Playing {game | people = coords :: game.people, turn = (opposite game.turn)}, Cmd.none)
          else
            (model, Cmd.none)
        BeginDrag ->
          (Playing {game | ball = updateBall BeginDrag game.ball}, Cmd.none)
        Drag xy ->
          (Playing {game | ball = updateBall (Drag xy) game.ball}, Cmd.none)
        Drop coords ->
          if validDrag coords game then
            (Playing {game | ball = updateBall (Drop coords) game.ball}, Cmd.none)
          else
            (Playing {game | ball = updateBall (Drop game.ball.coords) game.ball}, Cmd.none)
    _ ->
      (model, Cmd.none)

updateBall : Msg -> Ball -> Ball
updateBall msg ball =
  case msg of
    BeginDrag   -> {ball | dragging = True}
    Drag coords -> {ball | drawLoc = coords}
    Drop coords -> Ball False coords (loc coords)
    Click _     -> ball


subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing game ->
      case game.ball.dragging of
        True ->
          Sub.batch
          [ Browser.Events.onMouseDown (Json.Decode.map Drag decodeXY)
          , Browser.Events.onMouseMove (Json.Decode.map Drag decodeXY)
          , Browser.Events.onMouseUp   (Json.Decode.map Drop decodeCoords)
          ]
        False ->
          Browser.Events.onClick (Json.Decode.map Click decodeCoords)
    _ -> Sub.none

decodeX      = (Json.Decode.field "pageX" Json.Decode.int)
decodeY      = (Json.Decode.field "pageY" Json.Decode.int)
decodeCoords = (Json.Decode.map2 toCoords decodeX decodeY)
decodeXY     = (Json.Decode.map2 Coords decodeX decodeY)

-- idea: shift and resize game grid so that each coord is 1 unit apart to use round function and loc
toCoords : Int -> Int -> Coords
toCoords x y =
  let
    x_coord = round (toFloat x / square_width) - 1
    y_coord = round (toFloat y / square_width) - 1
  in
    Coords x_coord y_coord

view : Model -> Html Msg
view model =
  case model of
    Ready    game -> drawGame game
    Playing  game -> drawGame game
    Finished game -> drawGame game

drawGame : Game -> Html Msg
drawGame game =
  svg [width (String.fromInt (w + 2 * margin)), height (String.fromInt (h + 2 * margin))]
  ([background] ++ grid ++ endZones ++ people game.people ++ [drawBall game.ball])

horizontal : Int -> String -> Int -> Svg msg
horizontal y color width = line
  [ x1 <| String.fromInt <| square_width * (0 + 1)
  , y1 <| String.fromInt <| square_width * (y + 1)
  , x2 <| String.fromInt <| square_width * (14 + 1)
  , y2 <| String.fromInt <| square_width * (y + 1)
  , stroke color
  , strokeWidth <| String.fromInt width
  ] []

vertical : Int -> String -> Int -> Svg msg
vertical x color width = line
  [ x1 <| String.fromInt <| square_width * (x + 1)
  , y1 <| String.fromInt <| square_width * (1 + 1)
  , x2 <| String.fromInt <| square_width * (x + 1)
  , y2 <| String.fromInt <| square_width * (19 + 1)
  , stroke color
  , strokeWidth <| String.fromInt width
  ] []

stone : Coords -> String -> Svg msg
stone coords color = circle
  [ cx <| String.fromInt (loc <| coords).x
  , cy <| String.fromInt (loc <| coords).y
  , r  <| String.fromFloat (square_width * 0.45)
  , fill color
  ] []

background : Svg msg
background = rect
  [ x      <| String.fromInt 0
  , y      <| String.fromInt 0
  , width  <| String.fromInt (w + 2 * margin)
  , height <| String.fromInt (h + 2 * margin)
  , fill "BurlyWood"
  ] []

endZones : List (Svg msg)
endZones = [horizontal 0 "DarkBlue" 5, horizontal 20 "DarkBlue" 5]

drawBall : Ball -> Svg Msg
drawBall b =
  -- let
  --   x = if b.dragging then b.drawLoc.x else (loc <| b.coords).x
  --   y = if b.dragging then b.drawLoc.y else (loc <| b.coords).y
  --   radius = if b.dragging then square_width * 0.65 else square_width * 0.45
  -- in
  circle
  [ cx <| String.fromInt b.drawLoc.x
  , cy <| String.fromInt b.drawLoc.y
  , r  <| String.fromFloat (if b.dragging then square_width * 0.65 else square_width * 0.45)
  , fill "white"
  , Svg.Events.onMouseDown BeginDrag
  ] []

people : List Coords -> List (Svg msg)
people coords_list = List.map (\x -> stone x "black") coords_list

grid : List (Svg msg)
grid = (List.map (\x -> vertical x "black" 1) (List.range 0 14)) ++ (List.map (\y -> horizontal y "black" 1) (List.range 1 19))

loc : Coords -> Coords
loc coords = Coords (square_width * (coords.x + 1)) (square_width * (coords.y + 1))
