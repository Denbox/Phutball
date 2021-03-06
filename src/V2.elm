module V2 exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Browser
import Browser.Events exposing (onMouseDown, onMouseUp, onMouseMove)
import Html exposing (Html)
import Json.Decode exposing (..)

num_cols  = 15
num_rows  = 19
end_rows  = 2 -- endzones get added to rows
grid_size = 35
margin = grid_size
game_width  = grid_size * (num_cols-1) + 2 * margin
game_height = grid_size * (num_rows+end_rows-1) + 2 * margin

type alias Point =
  { x : Int
  , y : Int
  }

pAdd : Point -> Point -> Point
pAdd a b = Point (a.x + b.x) (a.y + b.y)

pSub : Point -> Point -> Point
pSub a b = Point (a.x - b.x) (a.y - b.y)

-- scalar multiplication
psMul : Point -> Int -> Point
psMul p a = Point (p.x * a) (p.y * a)

snapToGrid : Point -> Point
snapToGrid p =
  let
    snap x = round ((toFloat (x - margin)) / grid_size) * grid_size + margin
  in
    Point (snap p.x) (snap p.y)

inBoundsPerson : Point -> Bool
inBoundsPerson p =
  p.x >= margin && p.x <= game_width - margin && p.y >= margin + grid_size && p.y <= game_height - margin - grid_size

inBoundsBall : Point -> Bool
inBoundsBall p =
  p.x >= margin && p.x <= game_width - margin && p.y >= margin && p.y <= game_height - margin

type alias Game =
  { ball        : Ball
  , people      : List Point
  , turn        : Turn
  }

type alias Ball =
  { position : Point
  , history  : List Point
  }

type Turn
  = X
  | O

nextTurn turn =
  case turn of
    X -> O
    O -> X

type Mouse
  = Up
  | Pressing Point Point
  | Dragging Point Point

type Model
  = Playing Game Mouse

type Msg
  = MouseDown Point
  | MouseMove Point
  | MouseUp   Point

main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ =
  let
    ball_pos = Point (margin + 7 * grid_size) (margin + 9 * grid_size)
    game = Game (Ball ball_pos []) [] X
    mouse = Up
  in
    (Playing game mouse, Cmd.none)




-- make sure that mouse_down location = mouse_up location when trying to place people
-- fix path of ball drawing
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Playing game mouse ->
      case msg of
        -- only alter mouse state - game remains unchanged otherwise
        -- add a check here to see if the player is currently allowed to drag or place
        MouseDown point ->
          let
            start = snapToGrid point
            current = point
          in
            if snapToGrid point == game.ball.position then
              (Playing game (Dragging start current), Cmd.none)
            else
              (Playing game (Pressing start current), Cmd.none)
        -- again, only alter mouse state - rest of game remains unchanged
        MouseMove current ->
          case mouse of
            Up -> -- this is impossible. Should I have an error here?
              (Playing game mouse, Cmd.none)
            Dragging start _ ->
              (Playing game (Dragging start current), Cmd.none)
            Pressing start _ ->
              (Playing game (Pressing start current), Cmd.none)
        -- perform game update upon release
        -- try to place/jump, check for win condition, switch turns, etc
        MouseUp end ->
          let
            end_pos = snapToGrid end
          in
            case mouse of
              Up -> -- this is impossible. Should I have an error here?
                (Playing game mouse, Cmd.none)
              Dragging start_pos _ ->
                if start_pos == end_pos && List.length game.ball.history > 0 then
                  finishBallMove game
                else if validDrag game start_pos end_pos then
                  moveBall game start_pos end_pos
                else if validUndrag game start_pos end_pos then
                  undoBallMove game
                else
                  (Playing game Up, Cmd.none)
                  -- (Playing {game | ball = } Up, Cmd.none)
              Pressing start_pos _ ->
                if List.length game.ball.history > 0 then
                  (Playing game Up, Cmd.none) -- not allowed to place while moving
                else
                  placePerson game start_pos end_pos

-- make this swap turns if successful and player finishes
moveBall : Game -> Point -> Point -> (Model, Cmd Msg)
moveBall game start end =
  (Playing {game | ball = Ball end (start::game.ball.history)} Up, Cmd.none)

undoBallMove : Game -> (Model, Cmd Msg)
undoBallMove game =
  case game.ball.history of
    [] -> (Playing game Up, Cmd.none) -- this never happens
    (x::xs) -> (Playing {game | ball = Ball x xs} Up, Cmd.none) -- undo once
  -- (Playing {game | ball = Ball })

-- accepts points that move in cardinal directions, or along -1, 1 slope diagonals
validDragDirection : Point -> Point -> Bool
validDragDirection start end =
  let
    unique = start /= end
    diff = pSub end start
    horizontal_or_vertical = diff.x == 0 || diff.y == 0
    diagonal = abs diff.x == abs diff.y
  in
    unique && (horizontal_or_vertical || diagonal)

-- if validDragDirection start end, return all points on grid between start and end
pointsBetween : Point -> Point -> List Point
pointsBetween start end =
  if validDragDirection start end then
    let
      max_diff = Basics.max (abs (end.x - start.x)) (abs (end.y - start.y))
      num_steps = max_diff // grid_size
      -- this will never cause divide by zero because validDragDirection checks that start /= end
      shift = Point ((end.x - start.x) // num_steps) ((end.y - start.y) // num_steps)
    in
      List.map (\i -> pAdd start (psMul shift i)) (List.range 1 (num_steps - 1))
  else
    []

-- we also require at least 1 person in between
allPeople : List Point -> List Point -> Bool
allPeople points people =
  let
    all_points_are_people = (List.all (\x -> List.member x people) points)
    at_least_one_point = (List.length points) > 0
  in
   all_points_are_people && at_least_one_point

validDrag : Game -> Point -> Point -> Bool
validDrag game start end =
  let
    points_jumped_now = pointsBetween start end
    points_jumped_all = jumpedList game.ball
    valid_start = game.ball.position == start
    valid_end = inBoundsBall end && (not (List.member end game.people) || (List.member end points_jumped_all))
    valid_direction = validDragDirection start end
    all_people_between = allPeople points_jumped_now game.people
    no_double_jumped_people = List.all (\point -> not (List.member point points_jumped_all)) points_jumped_now
  in
    valid_start && valid_end && valid_direction && all_people_between && no_double_jumped_people

validUndrag : Game -> Point -> Point -> Bool
validUndrag game start end =
  case game.ball.history of
    [] -> False
    (x::xs) -> start == game.ball.position && end == x


-- make this swap turns if successful
placePerson : Game -> Point -> Point -> (Model, Cmd Msg)
placePerson game start end =
  let
    updated_people = (end::game.people)
    new_person = not (List.member end game.people) && end /= game.ball.position
  in
    if start == end && inBoundsPerson end && new_person then
      (Playing {game | people = updated_people, turn = nextTurn game.turn} Up, Cmd.none)
    else
      (Playing game Up, Cmd.none)

finishBallMove : Game -> (Model, Cmd Msg)
finishBallMove game =
  let
    jumped_people = jumpedList game.ball
    not_jumped person = not (List.member person jumped_people)
    remaining_people = List.filter not_jumped game.people
    ball = Ball game.ball.position []
  in
  (Playing (Game ball remaining_people (nextTurn game.turn)) Up, Cmd.none)

jumpedList : Ball -> List Point
jumpedList ball =
  let
    ball_path = ball.position::ball.history
  in
    case ball_path of
      [] -> [] -- this case never occurs
      [_] -> []
      (_::tail) -> List.concat (List.map2 pointsBetween ball_path tail)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing _ mouse ->
      case mouse of
        Up ->
          Sub.batch
          [ Browser.Events.onMouseDown (Json.Decode.map MouseDown decodeGridXY)
          , Browser.Events.onMouseUp   (Json.Decode.map MouseUp   decodeGridXY)
          ]
        _ ->
          Sub.batch
          [ Browser.Events.onMouseMove (Json.Decode.map MouseMove decodeXY)
          , Browser.Events.onMouseUp   (Json.Decode.map MouseUp   decodeGridXY)
          ]

decodeX  = (Json.Decode.field "pageX" Json.Decode.int)
decodeY  = (Json.Decode.field "pageY" Json.Decode.int)
decodeXY = (Json.Decode.map2 Point decodeX decodeY)
toGrid x y = snapToGrid <| Point x y
decodeGridXY = (Json.Decode.map2 toGrid decodeX decodeY)

view : Model -> Html Msg
view model =
  case model of
    Playing game mouse -> drawGame game mouse

drawGame : Game -> Mouse -> Html Msg
drawGame game mouse =
  svg [width <| String.fromInt game_width, height <| String.fromInt game_height]
  ([drawBackground] ++ drawGrid ++ drawEndZones ++ drawPeople game ++ (drawBall game.ball mouse))

rectangle : Int -> Int -> Int -> Int -> String -> Svg msg
rectangle left_x top_y w h color = rect
  [ x      <| String.fromInt left_x
  , y      <| String.fromInt top_y
  , width  <| String.fromInt w
  , height <| String.fromInt h
  , fill color
  ] []

drawBackground : Svg msg
drawBackground = rectangle 0 0 game_width game_height "BurlyWood"

horizontal : Int -> String -> Int -> Svg msg
horizontal y color width = line
  [ x1 <| String.fromInt <| grid_size * (0  + 1)
  , y1 <| String.fromInt <| grid_size * (y  + 1)
  , x2 <| String.fromInt <| grid_size * (14 + 1)
  , y2 <| String.fromInt <| grid_size * (y  + 1)
  , stroke color
  , strokeWidth <| String.fromInt width
  ] []

vertical : Int -> String -> Int -> Svg msg
vertical x color width = line
  [ x1 <| String.fromInt <| grid_size * (x  + 1)
  , y1 <| String.fromInt <| grid_size * (1  + 1)
  , x2 <| String.fromInt <| grid_size * (x  + 1)
  , y2 <| String.fromInt <| grid_size * (19 + 1)
  , stroke color
  , strokeWidth <| String.fromInt width
  ] []

drawCircle : Point -> Float -> String -> Float -> Svg msg
drawCircle point radius c o = circle
  [ cx <| String.fromInt point.x
  , cy <| String.fromInt point.y
  , r  <| String.fromFloat radius
  , fill c
  , opacity <| String.fromFloat o
  ] []

drawGrid : List (Svg msg)
drawGrid = (List.map (\x -> vertical x "black" 1) (List.range 0 (num_cols-1))) ++ (List.map (\y -> horizontal y "black" 1) (List.range 1 (num_rows-1+1)))

drawEndZones : List (Svg msg)
-- drawEndZones = [horizontal 0 "DarkBlue" 5, horizontal 20 "DarkBlue" 5, horizontal 1 "DarkBlue" 5, horizontal 19 "DarkBlue" 5]
drawEndZones =
  let
    fill_color = "none"
    stroke_color = "blue"
    dash_rule = "10,10"
  in
    [ rect
      [ x      <| String.fromInt (round (margin / 2))
      , y      <| String.fromInt (round (margin / 2))
      , width  <| String.fromInt (game_width - grid_size)
      , height <| String.fromInt (2 * grid_size)
      , fill fill_color
      , stroke stroke_color
      , strokeDasharray dash_rule
      , strokeWidth "2"] []
    , rect
    [ x      <| String.fromInt (round (margin / 2))
    , y      <| String.fromInt (game_height - round (margin / 2) - 2 * grid_size)
    , width  <| String.fromInt (game_width - grid_size)
    , height <| String.fromInt (2 * grid_size)
    , fill fill_color
    , stroke stroke_color
    , strokeDasharray dash_rule
    , strokeWidth "2"] []
    , horizontal 0 "black" 1
    , horizontal 20 "black" 1
    ]

drawPath : Ball -> Maybe Point -> String -> Int -> Svg msg
drawPath ball mouse_pos color width =
  case mouse_pos of
    Nothing ->
      let
        toStr p = String.fromInt p.x ++ "," ++ String.fromInt p.y
        str_points = String.join " " <| List.map toStr (ball.position::ball.history)
      in
        polyline [points str_points, stroke color, strokeWidth (String.fromInt width), fill "none", strokeLinecap "round", strokeLinejoin "round"] []
    Just pos ->
      let
        toStr p = String.fromInt p.x ++ "," ++ String.fromInt p.y
        str_points = String.join " " <| List.map toStr (pos::ball.position::ball.history)
      in
        polyline [points str_points, stroke color, strokeWidth (String.fromInt width), fill "none", strokeLinecap "round", strokeLinejoin "round"] []

drawBall : Ball -> Mouse -> List (Svg msg)
drawBall ball mouse =
  let
    line_color = "red"
    line_width = 10
    ball_color = "white"
    opacity    = 1
  in
    case mouse of
      Dragging start current ->
        [drawPath ball (Just current) line_color line_width, drawCircle current (0.65 * grid_size) ball_color opacity]
      _ ->
        [drawPath ball Nothing line_color line_width, drawCircle ball.position (0.45 * grid_size) ball_color opacity]

drawPeople : Game -> List (Svg msg)
drawPeople game =
  let
    people = game.people
    opacity person = if List.member person (jumpedList game.ball) then 0.5 else 1
  in
  List.map (\person -> drawCircle person (0.45 * grid_size) "black" (opacity person)) people
