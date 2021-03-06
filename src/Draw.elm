module Draw exposing (game_elements)
import Game exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Svg.Events exposing (custom)
import Json.Decode exposing (succeed)

-- draw : Game -> Maybe Point -> Html Msg
-- draw game mouse_point =
--   svg [width <| String.fromInt game_width, height <| String.fromInt game_height]

game_elements : Game -> Maybe Point -> List (Svg msg)
game_elements game mouse_point =
  [background] ++ endZones ++ (grid n_rows n_cols) ++ stones game.stones ++ jumpedStones game ++ [ballTrail game mouse_point, ball game mouse_point]

background : Svg msg
background =
  rect
    [ x "0"
    , y "0"
    , width  <| String.fromInt game_width
    , height <| String.fromInt game_height
    , fill "BurlyWood"
    ] []

endZones : List (Svg msg)
endZones =
  let
    top_zone =
      rect
      [ x      <| String.fromInt <| margin // 2
      , y      <| String.fromInt <| margin // 2
      , width  <| String.fromInt <| cell_size * n_cols
      , height <| String.fromInt <| cell_size * end_zone_length
      , fill "none"
      , stroke "blue"
      , strokeDasharray "13,6"
      , strokeWidth "2"
      ] []
    bottom_zone =
      rect
      [ x      <| String.fromInt <| margin // 2
      , y      <| String.fromInt <| game_height - cell_size * end_zone_length - margin // 2
      , width  <| String.fromInt <| cell_size * n_cols
      , height <| String.fromInt <| cell_size * end_zone_length
      , fill "none"
      , stroke "blue"
      , strokeDasharray "13,6"
      , strokeWidth "2"
      ] []
  in
    [top_zone, bottom_zone]

disk : Point -> String -> Float -> Float -> Svg msg
disk point color radius opac =
  circle
  [ cx <| String.fromInt <| point.x
  , cy <| String.fromInt <| point.y
  , r <| String.fromFloat <| cell_size * radius
  , fill color
  , opacity <| String.fromFloat opac
  ] []

stones : List Coord -> List (Svg msg)
stones coords =
  List.map (\c -> disk (Game.unSnap c) "black" 0.45 1) coords

jumpedStones : Game -> List (Svg msg)
jumpedStones game =
  let
    coords = List.concat (List.map2 Game.coordsBetween (game.ball.coord :: game.ball.history) game.ball.history)
  in
  List.map (\c -> disk (Game.unSnap c) "black" 0.45 0.5) coords

ballTrail : Game -> Maybe Point -> Svg msg
ballTrail game mouse_dragging =
  let
    pointToStr point = String.fromInt point.x ++ "," ++ String.fromInt point.y
    trail : List Point -> Svg msg
    trail history =
      polyline
      [ points <| String.join " " <| List.map pointToStr history
      , stroke "red"
      , strokeWidth <| String.fromInt 10
      , fill "none"
      , strokeLinecap "round"
      , strokeLinejoin "round"
      ] []
  in
    case mouse_dragging of
      Just point ->
        trail <| point :: (List.map Game.unSnap (game.ball.coord :: game.ball.history))
      Nothing ->
        trail <| List.map Game.unSnap (game.ball.coord :: game.ball.history)

ball : Game -> Maybe Point -> Svg msg
ball game mouse_dragging =
  case mouse_dragging of
    Just point ->
      disk point "white" 0.65 1
    Nothing ->
      disk (Game.unSnap game.ball.coord) "white" 0.45 1

lineSegment : Point -> Point -> String -> Int -> Svg msg
lineSegment start end color width =
  line
  [ x1 <| String.fromInt start.x
  , y1 <| String.fromInt start.y
  , x2 <| String.fromInt end.x
  , y2 <| String.fromInt end.y
  , stroke color
  , strokeWidth <| String.fromInt width
  ] []

grid : Int -> Int -> List (Svg msg)
grid num_rows num_cols =
  let
    left = List.map   (\y -> Coord 0 y)                                      <| List.range 0 (num_rows - 1)
    right = List.map  (\y -> Coord (num_cols - 1) y)                         <| List.range 0 (num_rows - 1)
    top = List.map    (\x -> Coord x (end_zone_length - 1))                  <| List.range 0 (num_cols - 1)
    bottom = List.map (\x -> Coord x (num_rows - 1 - (end_zone_length - 1))) <| List.range 0 (num_cols - 1)
    gridLine start end = lineSegment (Game.unSnap start) (Game.unSnap end) "black" 1
    vertical_lines   = List.map2 gridLine top bottom
    horizontal_lines = List.map2 gridLine left right
  in
    vertical_lines ++ horizontal_lines
