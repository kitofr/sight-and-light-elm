module Main exposing (..)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (style)
import Canvas exposing (Size, DrawOp(..), Canvas)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import Canvas.Events
import Color exposing (Color)


type alias Px =
    { x : Float
    , y : Float
    }


type alias Intersection =
    { x : Float
    , y : Float
    , param : Float
    }


type alias Ray =
    { a : Px
    , b : Px
    }


main =
    Html.beginnerProgram
        { model =
            ( Canvas.initialize (Size 640 360)
                |> Canvas.batch drawWalls
            , NoClick
            , Point.fromInts ( 0, 0 )
            )
        , view = view
        , update = update
        }


findClosestIntersection : Float -> Px -> Intersection
findClosestIntersection angle mousePos =
    let
        dx =
            cos (angle)

        dy =
            sin (angle)

        ray =
            createRay mousePos.x mousePos.y (mousePos.x + dx) (mousePos.y + dy)
    in
        List.foldl
            (\segment closest ->
                case getIntersection ray segment of
                    Just point ->
                        if point.param < closest.param then
                            point
                        else
                            closest

                    Nothing ->
                        closest
            )
            { x = 0, y = 0, param = 600 }
            segments


castRays : Px -> List Intersection
castRays mousePos =
    let
        steps =
            50

        stepAdjustment =
            pi * 2 / steps

        fullCircle =
            List.scanl (\_ b -> b + stepAdjustment) 0 (List.map toFloat (List.range 0 steps))
    in
        List.map (\angle -> findClosestIntersection angle mousePos) fullCircle


createRay : Float -> Float -> Float -> Float -> Ray
createRay ax ay bx by =
    Ray (Px ax ay) (Px bx by)


getIntersection : Ray -> Ray -> Maybe Intersection
getIntersection ray segment =
    let
        rPx =
            ray.a.x

        rPy =
            ray.a.y

        rDx =
            ray.b.x

        rDy =
            ray.b.y

        sPx =
            segment.a.x

        sPy =
            segment.a.y

        sDx =
            segment.b.x

        sDy =
            segment.b.y

        rMag =
            sqrt (rDx * rDx + rDy * rDy)

        sMag =
            sqrt (sDx * sDx + sDy * sDy)
    in
        if (rDx / rMag == sDx / sMag && rDy / rMag == sDy / sMag) then
            Nothing
        else
            let
                t2 =
                    (rDx * (sPy - rPy) + rDy * (rPx - sPx)) / (sDx * rDy - sDy * rDx)

                t1 =
                    (sPx + sDx * t2 - rPx) / rDx
            in
                if t1 < 0 then
                    Nothing
                else if t2 < 0 || t2 > 1 then
                    Nothing
                else
                    Just
                        { x = rPx + rDx * t1
                        , y = rPy + rDy * t1
                        , param = t1
                        }


segments : List Ray
segments =
    [ createRay 0 0 640 0
    , createRay 640 0 640 360
    , createRay 640 360 0 360
    , createRay 0 360 0 0
    , createRay 100 150 120 50
    , createRay 120 50 200 80
    , createRay 200 80 140 210
    , createRay 140 210 100 150
    , createRay 100 200 120 250
    , createRay 120 250 60 300
    , createRay 60 300 100 200
    , createRay 200 260 220 150
    , createRay 220 150 300 200
    , createRay 300 200 350 320
    , createRay 350 320 200 260
    , createRay 340 60 360 40
    , createRay 360 40 370 70
    , createRay 370 70 340 60
    , createRay 450 190 560 170
    , createRay 560 170 540 270
    , createRay 540 270 430 290
    , createRay 430 290 450 190
    , createRay 400 95 580 50
    , createRay 580 50 480 150
    , createRay 480 150 400 95
    ]


drawWalls : List DrawOp
drawWalls =
    List.concatMap (\{ a, b } -> line (Point.fromFloats ( a.x, a.y )) (Point.fromFloats ( b.x, b.y ))) segments


drawRays : Px -> List Intersection -> List DrawOp
drawRays mousePos rays =
    --  let _ = Debug.log "rays" rays
    --  in
    List.concatMap (\a -> line (Point.fromFloats ( mousePos.x, mousePos.y )) (Point.fromFloats ( a.x, a.y ))) rays


type ClickState
    = NoClick
    | FirstClick Point
    | Moving Point Point


type Msg
    = Click Point
    | Move Point


type alias Model =
    ( Canvas, ClickState, Point )


update : Msg -> Model -> Model
update message ( canvas, clickState, mousePos ) =
    let
        mouseDot =
            [ BeginPath
            , FillStyle Color.red
            , Arc mousePos 4 0 (2 * pi)
            , Fill
            ]

        clear =
            [ ClearRect (Point.fromInts ( 0, 0 )) { width = 640, height = 360 } ]

        toPx : Point -> Px
        toPx p =
            let
                ( x, y ) =
                    Point.toFloats p
            in
                (Px x y)

        rays =
            castRays (toPx mousePos)

        canvas_ =
            canvas
                |> Canvas.batch (List.concat [ clear, mouseDot, drawWalls, drawRays (toPx mousePos) rays ])
    in
        case message of
            Click position ->
                case clickState of
                    NoClick ->
                        ( canvas_, FirstClick position, mousePos )

                    FirstClick p1 ->
                        ( canvas_, clickState, mousePos )

                    Moving p0 p1 ->
                        ( drawLine p0 p1 canvas_, NoClick, mousePos )

            Move position ->
                case clickState of
                    NoClick ->
                        ( canvas_, NoClick, position )

                    FirstClick p0 ->
                        ( canvas_, Moving p0 position, position )

                    Moving p0 _ ->
                        ( canvas_, Moving p0 position, position )


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
handleClickState ( canvas, clickState, mousePos ) =
    case clickState of
        NoClick ->
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
