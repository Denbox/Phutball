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

snapToGrid : Point -> Point
snapToGrid p =
  let
    snap x = round ((toFloat (x - margin)) / grid_size) * grid_size + margin
  in
    Point (snap p.x) (snap p.y)

type alias Game =
  { ball    : Ball
  , people  : List Point
  , turn    : Turn
  }

type alias Ball =
  { position : Point
  , history  : List Point
  , dragging : Bool
  }

type Turn
  = X
  | O

type alias Mouse =
  { pressed  : Bool
  , position : Maybe Point
  }

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
    game = Game (Ball ball_pos [] False) [] X
    mouse = Mouse False Nothing
  in
    (Playing game mouse, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Playing game mouse ->
      case msg of
        MouseDown point ->
          let
            new_mouse = {mouse | pressed = True}
          in
            if snapToGrid point == game.ball.position then
              let
                ball = game.ball
                new_ball = {ball | dragging = True}
              in
                (Playing {game | ball = new_ball} new_mouse, Cmd.none)
            else
              (model, Cmd.none)
        MouseMove point ->
          if game.ball.dragging then
            let
              ball = game.ball
              new_ball = {ball | position = point}
            in
              (Playing {game | ball = new_ball} mouse, Cmd.none)
          else
            (model, Cmd.none)
        MouseUp point ->
          let
            ball = game.ball
            new_mouse = {mouse | pressed = False}
            new_ball = {ball | dragging = False, position = snapToGrid (ball.position)}
          in
            (Playing {game | ball = new_ball} new_mouse, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing _ mouse ->
      if mouse.pressed then
        Sub.batch
        [ Browser.Events.onMouseMove (Json.Decode.map MouseMove decodeXY)
        , Browser.Events.onMouseUp   (Json.Decode.map MouseUp   decodeGridXY)
        ]
      else
        Sub.batch
        [ Browser.Events.onMouseDown (Json.Decode.map MouseDown decodeGridXY)
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
  ([drawBackground] ++ drawGrid ++ drawEndZones ++ (drawBall game.ball mouse))

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

drawCircle : Point -> Float -> String -> Svg msg
drawCircle point radius c = circle
  [ cx <| String.fromInt point.x
  , cy <| String.fromInt point.y
  , r  <| String.fromFloat radius
  , fill c
  ] []

drawGrid : List (Svg msg)
drawGrid = (List.map (\x -> vertical x "black" 1) (List.range 0 (num_cols-1))) ++ (List.map (\y -> horizontal y "black" 1) (List.range 1 (num_rows-1+1)))

drawEndZones : List (Svg msg)
drawEndZones = [horizontal 0 "DarkBlue" 5, horizontal 20 "DarkBlue" 5, horizontal 1 "DarkBlue" 5, horizontal 19 "DarkBlue" 5]
-- drawEndZones = [rectangle]

drawPath : List Point -> String -> Int -> Svg msg
drawPath points_list color width =
  let
    toStr p = String.fromInt p.x ++ "," ++ String.fromInt p.y
    str_points = String.join " " <| List.map toStr points_list
  in
    polyline [points str_points, stroke color, strokeWidth (String.fromInt width), fill "none", strokeLinecap "round", strokeLinejoin "round"] []

drawBall : Ball -> Mouse -> List (Svg msg)
drawBall ball mouse =
  let
    line_color = "red"
    line_width = 10
    ball_color = "white"
  in
    case mouse.pressed of
      True ->
        case mouse.position of
          Nothing ->
            [drawPath ball.history line_color line_width, drawCircle ball.position (0.65 * grid_size) ball_color]
          Just point ->
            [drawPath (ball.history ++ [point]) line_color line_width, drawCircle ball.position (0.65 * grid_size) ball_color]
      False ->
        [drawPath ball.history line_color line_width, drawCircle ball.position (0.45 * grid_size) ball_color]
