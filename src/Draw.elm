module Draw exposing (..)
import Game exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)

-- draw : List Coord -> List Point -> Point -> Bool -> Html msg
-- draw stone_coords ball_history ball_point dragging =
draw : Game -> Html msg
draw game =
  svg [width <| String.fromInt game_width, height <| String.fromInt game_height]
  ([background] ++ endZones ++ (grid n_rows n_cols) ++ stones game.stones ++ [ballTrail game.ball, drawBall game.ball])


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
      , width  <| String.fromInt <| unit * n_cols
      , height <| String.fromInt <| unit * end_zone_length
      , fill "none"
      , stroke "blue"
      , strokeDasharray "13,6"
      , strokeWidth "2"
      ] []
    bottom_zone =
      rect
      [ x      <| String.fromInt <| margin // 2
      , y      <| String.fromInt <| game_height - unit * end_zone_length - margin // 2
      , width  <| String.fromInt <| unit * n_cols
      , height <| String.fromInt <| unit * end_zone_length
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
  , r <| String.fromFloat <| unit * radius
  , fill color
  , opacity <| String.fromFloat opac
  ] []

stones : List Coord -> List (Svg msg)
stones coords =
  List.map (\c -> disk (Game.unSnap c) "black" 0.45 1) coords

ballTrail : Ball -> Svg msg
ballTrail ball =
  let
    pointToStr point = String.fromInt point.x ++ "," ++ String.fromInt point.y
    ball_history_points = (getBallPoint ball) :: ball.history
    ball_history_str = String.join " " <| List.map pointToStr ball_history_points
  in
    polyline
    [ points ball_history_str
    , stroke "red"
    , strokeWidth <| String.fromInt 10
    , fill "none"
    , strokeLinecap "round"
    , strokeLinejoin "round"
    ] []

drawBall : Game.Ball -> Svg msg
drawBall ball =
  let
    scale = if Game.dragging ball then 0.65 else 0.45
    point = getBallPoint ball
  in
    disk point "white" scale 1

jumpedStones : List Coord -> List (Svg msg)
jumpedStones coords =
  List.map (\c -> disk (Game.unSnap c) "black" 0.45 0.5) coords

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
