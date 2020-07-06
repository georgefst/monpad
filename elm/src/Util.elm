module Util exposing (..)

import Color
import Color.Interpolate exposing (Space(..), interpolate)
import Html
import Html.Attributes as Attr
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


darkColor : Color.Color -> Color.Color
darkColor c =
    interpolate RGB c Color.black 0.6


viewBox : Float -> Float -> Float -> Float -> Html.Attribute msg
viewBox x y w h =
    Attr.attribute "viewBox" <| String.join " " <| List.map String.fromFloat [ x, y, w, h ]


applyWhen : Bool -> (a -> a) -> a -> a
applyWhen b f x =
    if b then
        f x

    else
        x
