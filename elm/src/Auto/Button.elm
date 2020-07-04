module Auto.Button exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Math.Vector2
import Util


type Button 
    = CircleButton Float
    | RectangleButton Math.Vector2.Vec2


decode : Json.Decode.Decoder Button
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "CircleButton" ->
            Json.Decode.succeed CircleButton |>
            Json.Decode.Pipeline.required "contents" Json.Decode.float
        
        "RectangleButton" ->
            Json.Decode.succeed RectangleButton |>
            Json.Decode.Pipeline.required "contents" Util.decodeVec2
        
        _ ->
            Json.Decode.fail "No matching constructor")