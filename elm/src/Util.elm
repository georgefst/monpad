module Util exposing (..)

import Auto.IntVec2 exposing (IntVec2)
import Browser.Dom exposing (getViewport)
import Color
import Color.Interpolate exposing (Space(..), interpolate)
import Html
import Html.Attributes as Attr
import Json.Encode
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Task exposing (Task)


both : (a -> b) -> ( a, a ) -> ( b, b )
both f ( x, y ) =
    ( f x, f y )


maybe : a -> (b -> a) -> Maybe b -> a
maybe e f x =
    case x of
        Just x1 ->
            f x1

        Nothing ->
            e


either : (x -> r) -> (a -> r) -> Result x a -> r
either f g x =
    case x of
        Err x1 ->
            f x1

        Ok x1 ->
            g x1


limit : ( comparable, comparable ) -> comparable -> comparable
limit ( l, u ) =
    max l << min u


unVec2 : Vec2 -> ( Float, Float )
unVec2 v =
    ( getX v, getY v )


encodeVec2 : Vec2 -> Json.Encode.Value
encodeVec2 v =
    case unVec2 v of
        ( x, y ) ->
            Json.Encode.list identity [ Json.Encode.float x, Json.Encode.float y ]


zeroVec2 : Vec2
zeroVec2 =
    vec2 0 0


{-| Similar to 'Vec2.fromRecord'.
-}
vec2FromIntRecord : { a | x : Int, y : Int } -> Vec2
vec2FromIntRecord v =
    vec2 (toFloat v.x) (toFloat v.y)


darkColor : Color.Color -> Color.Color
darkColor c =
    interpolate RGB c Color.black 0.6


viewBox : Int -> Int -> Int -> Int -> Html.Attribute msg
viewBox x y w h =
    Attr.attribute "viewBox" <| String.join " " <| List.map String.fromInt [ x, y, w, h ]


applyWhen : Bool -> (a -> a) -> a -> a
applyWhen b f x =
    if b then
        f x

    else
        x


getViewportSize : Task x IntVec2
getViewportSize =
    getViewport |> Task.map (\viewport -> { x = round viewport.viewport.width, y = round viewport.viewport.height })


scaleVec2 : { a | sfX : Float, sfY : Float } -> Vec2 -> Vec2
scaleVec2 factors v =
    vec2 (factors.sfX * getX v) (factors.sfY * getY v)
