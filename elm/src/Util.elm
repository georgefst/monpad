module Util exposing (..)

import Auto.IntVec2 exposing (IntVec2)
import Browser.Dom exposing (getViewport)
import Color
import Color.Interpolate exposing (Space(..), interpolate)
import Html
import Html.Attributes as Attr
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Round exposing (roundNum)
import Task exposing (Task)


both : (a -> b) -> ( a, a ) -> ( b, b )
both f ( x, y ) =
    ( f x, f y )


bool : a -> a -> Bool -> a
bool f t x =
    if x then
        t

    else
        f


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


unVec2 : Vec2 -> ( Float, Float )
unVec2 v =
    ( getX v, getY v )


encodeFloatRounded : Float -> Json.Encode.Value
encodeFloatRounded x =
    -- TODO we might not always want 4 digits - make this configurable
    Json.Encode.float <| roundNum 4 x


encodeVec2 : Vec2 -> Json.Encode.Value
encodeVec2 v =
    case unVec2 v of
        ( x, y ) ->
            Json.Encode.list identity [ encodeFloatRounded x, encodeFloatRounded y ]


decodeVec2 : Json.Decode.Decoder Vec2
decodeVec2 =
    Json.Decode.succeed vec2
        |> Json.Decode.Pipeline.required "x" Json.Decode.float
        |> Json.Decode.Pipeline.required "y" Json.Decode.float


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


performCmd : a -> Cmd a
performCmd =
    Task.perform identity << Task.succeed


getViewportSize : Task x IntVec2
getViewportSize =
    getViewport |> Task.map (\viewport -> { x = round viewport.viewport.width, y = round viewport.viewport.height })


scaleVec2 : { a | sfX : Float, sfY : Float } -> Vec2 -> Vec2
scaleVec2 factors v =
    vec2 (factors.sfX * getX v) (factors.sfY * getY v)


angleVec2 : Vec2 -> Float
angleVec2 v =
    atan (getY v / getX v)


mapX : (Float -> Float) -> Vec2 -> Vec2
mapX f v =
    vec2 (f <| getX v) (getY v)


mapY : (Float -> Float) -> Vec2 -> Vec2
mapY f v =
    vec2 (getX v) (f <| getY v)
