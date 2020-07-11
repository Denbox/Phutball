module Game exposing (Coord, Point, Game, Ball, initialize, testMove, validBallMove, validPlacement, validBallTurnFinish, moveBall, undoBallMove, finishBallTurn, placeStone, coordsBetween, snap, unSnap, n_rows, n_cols, end_zone_length, cell_size, margin, game_width, game_height)

import List exposing (member, range, map, filter, filterMap, head, tail)
import Maybe exposing (withDefault)

n_rows = 19
n_cols = 15
end_zone_length = 2

cell_size = 35 -- the length of an edge of a grid cell
margin = cell_size
game_width  = margin * 2 + cell_size * (n_cols - 1)
game_height = margin * 2 + cell_size * (n_rows - 1)

-- used for game logic
type alias Coord =
  { x : Int
  , y : Int
  }

-- coord addition
cAdd : Coord -> Coord -> Coord
cAdd a b =
  Coord (a.x + b.x) (a.y + b.y)

-- coord subtraction
cSub : Coord -> Coord -> Coord
cSub a b =
  Coord (a.x - b.x) (a.y - b.y)

-- scalar multiplication
sMul : Coord -> Int -> Coord
sMul coord scalar =
  Coord (coord.x * scalar) (coord.y * scalar)

sign : Int -> Int
sign x =
  if x >= 0 then 1 else -1

makeUnitLength : Coord -> Maybe Coord
makeUnitLength coord =
  case (coord.x, coord.y) of
    (0, 0) ->
      Nothing
    (0, y) ->
      Just <| Coord 0 (sign y)
    (x, 0) ->
      Just <| Coord (sign x) 0
    (x, y) ->
      if abs x /= abs y then
        Nothing
      else
        Just <| Coord (sign x) (sign y)

-- used for graphics
type alias Point =
  { x : Int
  , y : Int
  }

type Turn
  = X
  | O

nextTurn : Turn -> Turn
nextTurn turn =
  case turn of
    X ->
      O
    O ->
      X

type alias Ball =
  { coord : Coord
  , history : List Coord
  }

type alias Game =
  { ball : Ball
  , stones : List Coord
  , turn : Turn
  }

validBallCoord c =
  member c.x (range 0 <| n_cols - 1) && member c.y (range 0 <| n_rows - 1)

validStoneCoord c =
  member c.x (range 0 <| n_cols - 1) && member c.y (range (end_zone_length - 1) <| n_rows - 1 - (end_zone_length - 1))

unSnap : Coord -> Point
unSnap c =
  Point (c.x * cell_size + margin) (c.y * cell_size + margin)

snap : Point -> Coord
snap p =
  Coord (round ((toFloat p.x - margin) / cell_size)) (round ((toFloat p.y - margin) / cell_size))

testMove : Game -> Coord -> Maybe Coord
testMove game direction =
  let
    moveHelper : Game -> Coord -> Coord -> Maybe Coord
    moveHelper g dir current =
      if current == game.ball.coord then
        moveHelper g dir (cAdd current dir)
      else if not <| validBallCoord current then
        Nothing
      else if not <| member current g.stones then
        if (cAdd game.ball.coord dir) == current then
          Nothing
        else
          Just current
      else
        moveHelper g dir (cAdd current dir)
  in
    moveHelper game direction game.ball.coord

coordsBetween : Coord -> Coord -> List Coord
coordsBetween start end =
  let
    diff = cSub end start
    direction = makeUnitLength diff
    steps_to_end = max (abs diff.x) (abs diff.y)
  in
    case direction of
      Nothing ->
        []
      Just dir ->
        -- we go from 1 to steps_to_end - 1 to exclude end points
        map (\i -> cAdd start <| sMul dir i) (range 1 (steps_to_end - 1))

validBallMove : Game -> Coord -> Bool
validBallMove game landing_coord =
  let
  -- up, down, left, right, and the diagonals (of the first 4 directions rotated 45 degrees)
    move_directions = [Coord 1 0, Coord 0 1, Coord -1 0, Coord 0 -1, Coord 1 1, Coord 1 -1, Coord -1 1, Coord -1 -1]
    possible_moves = filterMap (testMove game) move_directions
  in
    member landing_coord possible_moves

validPlacement : Game -> Coord -> Bool
validPlacement game coord =
  let
    no_stone_there = not <| member coord game.stones
    not_the_ball = coord /= game.ball.coord
    valid_location = validStoneCoord coord
    not_moving_ball = game.ball.history == []
  in
    no_stone_there && not_the_ball && valid_location && not_moving_ball

moveBall : Game -> Coord -> Result String Game
moveBall game landing_coord =
  let
    b = game.ball
    updated_ball = Ball landing_coord (b.coord::b.history)
    jumped_stones = coordsBetween b.coord landing_coord
    updated_stones = filter (\stone -> not <| member stone jumped_stones) game.stones
  in
    if validBallMove game landing_coord then
      Ok {game | ball = updated_ball, stones = updated_stones} -- turn only switches when we finish a ball move
    else
      Err "Invalid ball move"

undoBallMove : Game -> Coord -> Result String Game
undoBallMove game coord =
  if game.ball.coord /= coord then
    Err "Can't undo ball move"
  else
    case game.ball.history of
      (x::xs) ->
        Ok {game | ball = (Ball x xs), stones = (game.stones ++ (coordsBetween x coord))}
      [] ->
        Err "Can't undo ball move"
  -- let
  --   case game.ball.history of
  --     (x:xs) ->
  --   undo_ball = Ball coord (withDefault [] <| tail game.ball.history)
  --   jumped_stones = coordsBetween game.ball.coord coord
  --   undo_stones = game.stones ++ jumped_stones
  -- in
  --   if game.ball.coord == coord then
  --     Ok {game | ball = undo_ball, stones = undo_stones}
  --   else
  --     Err "Can't undo ball move"

validBallTurnFinish : Game -> Coord -> Bool
validBallTurnFinish game coord =
  game.ball.history /= [] && game.ball.coord == coord

finishBallTurn : Game -> Coord -> Result String Game
finishBallTurn game coord =
  let
    updated_ball = Ball game.ball.coord []
    updated_turn = nextTurn game.turn
  in
    if game.ball.history /= [] && coord == game.ball.coord then
      Ok {game | ball = updated_ball, turn = updated_turn}
    else
      Err "Invalid turn finish"

placeStone : Game -> Coord -> Result String Game
placeStone game coord =
  if validPlacement game coord then
    let
      updated_stones = coord::game.stones
      updated_turn = nextTurn game.turn
    in
      Ok {game | stones = updated_stones, turn = updated_turn}
  else
    Err "Invalid stone placement"

initialize : Game
initialize = Game (Ball (Coord 7 9) []) [Coord 5 2, Coord 8 9, Coord 9 9, Coord 8 10, Coord 1 2] X
