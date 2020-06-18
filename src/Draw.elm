module Draw exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)

-- used for game logic
type alias Coord =
  { x : Int
  , y : Int
  }

-- used for graphics
type alias Point =
  { x : Int
  , y : Int
  }

n_rows = 19
n_cols = 15
end_zone_length = 2

unit = 35 -- the length of an edge of a grid cell
margin = unit
game_width  = margin * 2 + unit * (n_cols - 1)
game_height = margin * 2 + unit * (n_rows - 1)

draw : List Coord -> List Point -> Point -> Bool -> Html msg
draw stone_coords ball_history ball_point dragging =
-- draw game =
  svg [width <| String.fromInt game_width, height <| String.fromInt game_height]
  ([background] ++ endZones ++ (grid n_rows n_cols) ++ stones stone_coords ++ [ballTrail ball_history, ball ball_point dragging])

unSnap : Coord -> Point
unSnap c =
  Point (c.x * unit + margin) (c.y * unit + margin)

snap : Point -> Coord
snap p =
  Coord (p.x // unit - margin) (p.y // unit - margin)

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
  List.map (\c -> disk (unSnap c) "black" 0.45 1) coords

ballTrail : List Point -> Svg msg
ballTrail ball_history_points =
  let
    pointToStr point = String.fromInt point.x ++ "," ++ String.fromInt point.y
    -- we need to add the current position to this listToStr function
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

ball : Point -> Bool -> Svg msg
ball point dragging =
  let
    scale = if dragging then 0.65 else 0.45
  in
    disk point "white" scale 1
  -- disk (getBallPoint b) "white" 0.45 1
  -- case b.point of
  --   Nothing ->
  --     disk (unSnap b.coord) "white" 0.45 1
  --   Just point ->
  --     disk point "white" 0.65 1

jumpedStones : List Coord -> List (Svg msg)
jumpedStones coords =
  List.map (\c -> disk (unSnap c) "black" 0.45 0.5) coords

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
    gridLine start end = lineSegment (unSnap start) (unSnap end) "black" 1
    vertical_lines   = List.map2 gridLine top bottom
    horizontal_lines = List.map2 gridLine left right
  in
    vertical_lines ++ horizontal_lines
