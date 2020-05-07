module Util exposing (..)

import Color
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Math.Vector2 as Vec2


both : (a -> b) -> ( a, a ) -> ( b, b )
both f ( x, y ) =
    ( f x, f y )


limit : ( comparable, comparable ) -> comparable -> comparable
limit ( l, u ) =
    max l << min u


unVec2 : Vec2.Vec2 -> ( Float, Float )
unVec2 v =
    ( Vec2.getX v, Vec2.getY v )


encodeVec2 : Vec2.Vec2 -> Json.Encode.Value
encodeVec2 v =
    case unVec2 v of
        ( x, y ) ->
            Json.Encode.list identity [ Json.Encode.float x, Json.Encode.float y ]


decodeVec2 : Json.Decode.Decoder Vec2.Vec2
decodeVec2 =
    Json.Decode.succeed Vec2.vec2
        |> Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.float)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float)


darkColor : Color.Color -> Color.Color
darkColor c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    Color.fromRgba
        { red = red / 2
        , green = green / 2
        , blue = blue / 2
        , alpha = alpha / 2
        }