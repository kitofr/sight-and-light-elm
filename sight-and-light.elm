module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (style)
import Canvas exposing (Size, DrawOp(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import Canvas.Events
import Color exposing (Color)


main =
    Html.beginnerProgram
        { model =
            ( Canvas.initialize (Size 640 360)
                |> Canvas.batch drawWalls
            , Nothing
            )
        , view = view
        , update = update
        }


drawWalls : List DrawOp
drawWalls =
    List.concat
        [ -- Border
          line (Point.fromInts ( 0, 0 )) (Point.fromInts ( 640, 0 ))
        , line (Point.fromInts ( 640, 0 )) (Point.fromInts ( 640, 360 ))
        , line (Point.fromInts ( 640, 360 )) (Point.fromInts ( 0, 360 ))
        , line (Point.fromInts ( 0, 360 )) (Point.fromInts ( 0, 0 ))
        , -- Polygon #1
          line (Point.fromInts ( 100, 150 )) (Point.fromInts ( 120, 50 ))
        , line (Point.fromInts ( 120, 50 )) (Point.fromInts ( 200, 80 ))
        , line (Point.fromInts ( 200, 80 )) (Point.fromInts ( 140, 210 ))
        , line (Point.fromInts ( 140, 210 )) (Point.fromInts ( 100, 150 ))
        , -- Polygon #2
          line (Point.fromInts ( 100, 200 )) (Point.fromInts ( 120, 250 ))
        , line (Point.fromInts ( 120, 250 )) (Point.fromInts ( 60, 300 ))
        , line (Point.fromInts ( 60, 300 )) (Point.fromInts ( 100, 200 ))
        , -- Polygon #3
          line (Point.fromInts ( 200, 260 )) (Point.fromInts ( 220, 150 ))
        , line (Point.fromInts ( 220, 150 )) (Point.fromInts ( 300, 200 ))
        , line (Point.fromInts ( 300, 200 )) (Point.fromInts ( 350, 320 ))
        , line (Point.fromInts ( 350, 320 )) (Point.fromInts ( 200, 260 ))
        , -- Polygon #4
          line (Point.fromInts ( 340, 60 )) (Point.fromInts ( 360, 40 ))
        , line (Point.fromInts ( 360, 40 )) (Point.fromInts ( 370, 70 ))
        , line (Point.fromInts ( 370, 70 )) (Point.fromInts ( 340, 60 ))
        , -- Polygon #5
          line (Point.fromInts ( 450, 190 )) (Point.fromInts ( 560, 170 ))
        , line (Point.fromInts ( 560, 170 )) (Point.fromInts ( 540, 270 ))
        , line (Point.fromInts ( 540, 270 )) (Point.fromInts ( 430, 290 ))
        , line (Point.fromInts ( 430, 290 )) (Point.fromInts ( 450, 190 ))
        , -- Polygon #6
          line (Point.fromInts ( 400, 95 )) (Point.fromInts ( 580, 50 ))
        , line (Point.fromInts ( 580, 50 )) (Point.fromInts ( 480, 150 ))
        , line (Point.fromInts ( 480, 150 )) (Point.fromInts ( 400, 95 ))
        ]


type ClickState
    = Nothing
    | FirstClick Point
    | Moving Point Point


type Msg
    = Click Point
    | Move Point


type alias Model =
    ( Canvas, ClickState )


update : Msg -> Model -> Model
update message ( canvas, clickState ) =
    case message of
        Click position ->
            case clickState of
                Nothing ->
                    ( canvas, FirstClick position )

                FirstClick p1 ->
                    ( canvas, clickState )

                Moving p0 p1 ->
                    ( drawLine p0 p1 canvas, Nothing )

        Move position ->
            case clickState of
                Nothing ->
                    ( canvas, Nothing )

                FirstClick p0 ->
                    ( canvas, Moving p0 position )

                Moving p0 _ ->
                    ( canvas, Moving p0 position )


view : Model -> Html Msg
view model =
    Canvas.toHtml
        [ style
            [ ( "border", "1px solid black" ) ]
        , Canvas.Events.onMouseDown Click
        , Canvas.Events.onMouseMove Move
        ]
        (handleClickState model)


handleClickState : Model -> Canvas
handleClickState ( canvas, clickState ) =
    case clickState of
        Nothing ->
            canvas

        FirstClick _ ->
            canvas

        Moving p0 p1 ->
            drawLine p0 p1 canvas


line : Point -> Point -> List DrawOp
line p0 p1 =
    [ BeginPath
    , LineWidth 1
    , LineCap "round"
    , MoveTo p0
    , LineTo p1
    , Stroke
    ]


drawLine : Point -> Point -> Canvas -> Canvas
drawLine p0 p1 =
    Canvas.batch (line p0 p1)
