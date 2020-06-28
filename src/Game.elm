module Game exposing (Coord, Point, Game, Ball, initialize, testMove, validBallMove, validPlacement, validBallTurnFinish, dragBall, moveBall, finishBallTurn, placeStone, enterMousePress, getBallPoint, coordsBetween, setBallPoint, snap, unSnap, n_rows, n_cols, end_zone_length, cell_size, margin, game_width, game_height)

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
  , point : Maybe Point -- used if ball is currently being moved
  }

type alias Game =
  { ball : Ball
  , stones : List Coord
  , turn : Turn
  }

getBallPoint : Game -> Point
getBallPoint game =
  case game.ball.point of
    Nothing ->
      unSnap game.ball.coord
    Just point ->
      point

setBallPoint : Game -> Point -> Game
setBallPoint game point =
  let
    b = game.ball
    updated_ball = {b | point = Just point}
  in
    {game | ball = updated_ball}

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

moveBall : Game -> Coord -> Game
moveBall game landing_coord =
  let
    b = game.ball
    undo_ball = Ball landing_coord (withDefault [] <| tail b.history) Nothing
    save_ball = Ball landing_coord (b.coord::b.history) Nothing
    normal_ball = {b | point = Nothing}
    jumped_stones = coordsBetween b.coord landing_coord
    undo_stones = game.stones ++ jumped_stones
    redo_stones = filter (\stone -> not <| member stone jumped_stones) game.stones
    updated_ball   = if head b.history == Just landing_coord then undo_ball else save_ball
    updated_stones = if head b.history == Just landing_coord then undo_stones else redo_stones
  in
    if validBallMove game landing_coord then
      {game | ball = updated_ball, stones = updated_stones} -- turn only switches when we finish a ball move
    else
      {game | ball = normal_ball}

-- this is used to update the grpahics of the ball position
dragBall : Game -> Point -> Game
dragBall game point =
  let
    b = game.ball
    dragged_ball = {b | point = Just point}
  in
  {game | ball = dragged_ball}

enterMousePress : Game -> Point -> Game
enterMousePress game point =
  let
    b = game.ball
    updated_ball = {b | point = Just point}
  in
    if snap point == game.ball.coord then
      {game | ball = updated_ball}
    else
      game



validBallTurnFinish : Game -> Coord -> Bool
validBallTurnFinish game coord =
  game.ball.history /= [] && game.ball.coord == coord

finishBallTurn : Game -> Game
finishBallTurn game =
  let
    b = game.ball
    updated_ball = Ball game.ball.coord [] Nothing
    updated_turn = nextTurn game.turn
    normal_ball = {b | point = Nothing}
  in
    if game.ball.history /= [] then
      {game | ball = updated_ball, turn = updated_turn}
    else
      {game | ball = normal_ball}

placeStone : Game -> Coord -> Game
placeStone game coord =
  if validPlacement game coord then
    let
      updated_stones = coord::game.stones
      updated_turn = nextTurn game.turn
    in
      {game | stones = updated_stones, turn = updated_turn}
  else
    game

initialize : Game
initialize = Game (Ball (Coord 7 9) [] Nothing) [Coord 5 2, Coord 8 9, Coord 9 9, Coord 8 10, Coord 1 2] X
