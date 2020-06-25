module Game exposing (..)

n_rows = 19
n_cols = 15
end_zone_length = 2

unit = 35 -- the length of an edge of a grid cell
margin = unit
game_width  = margin * 2 + unit * (n_cols - 1)
game_height = margin * 2 + unit * (n_rows - 1)

-- used for game logic
type alias Coord =
  { x : Int
  , y : Int
  }

cAdd : Coord -> Coord -> Coord
cAdd a b =
  Coord (a.x + b.x) (a.y + b.y)

cSub : Coord -> Coord -> Coord
cSub a b =
  Coord (a.x - b.x) (a.y - b.y)

-- used for graphics
type alias Point =
  { x : Int
  , y : Int
  }

type Turn
  = X
  | O

type alias Ball =
  { coord : Coord
  , history : List Coord
  , point : Maybe Point -- used if ball is currently being moved
  }

type alias Game =
  { ball : Ball
  , stones : List Coord
  , turn : Turn
  }

getBallPoint : Ball -> Point
getBallPoint b =
  case b.point of
    Nothing ->
      unSnap b.coord
    Just point ->
      point

dragging : Ball -> Bool
dragging ball =
  case ball.point of
    Nothing ->
      False
    Just _ ->
      True

validBallCoord c =
  List.member c.x (List.range 0 <| n_cols - 1) && List.member c.y (List.range 0 <| n_rows - 1)

validStoneCoord c =
  List.member c.x (List.range 0 <| n_cols - 1) && List.member c.y (List.range (end_zone_length - 1) <| n_rows - 1 - (end_zone_length - 1))

unSnap : Coord -> Point
unSnap c =
  Point (c.x * unit + margin) (c.y * unit + margin)

snap : Point -> Coord
snap p =
  Coord (p.x // unit - margin) (p.y // unit - margin)

tryMove : Game -> Coord -> Maybe Coord
tryMove game direction =
  let
    moveHelper : Game -> Coord -> Coord -> Maybe Coord
    moveHelper g dir current =
      if not <| validBallCoord current then
        Nothing
      else if not <| List.member current g.stones then
        Just current
      else
        moveHelper g dir (cAdd current direction)
  in
    moveHelper game direction game.ball.coord


validBallMove : Game -> Coord -> Bool
validBallMove game landing_coord =
  let
  -- up, down, left, right, and the diagonals (of the first 4 directions rotated 45 degrees)
    move_directions = [Coord 1 0, Coord 0 1, Coord -1 0, Coord 0 -1, Coord 1 1, Coord 1 -1, Coord -1 1, Coord -1 -1]
    possible_moves = List.filterMap (\dir -> tryMove game dir) move_directions
  in
    List.member landing_coord possible_moves
