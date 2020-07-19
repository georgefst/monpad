module Auto.Shape exposing (..)

import Auto.IntVec2
import Json.Decode
import Json.Decode.Pipeline


type Shape 
    = Circle Int
    | Rectangle Auto.IntVec2.IntVec2


decode : Json.Decode.Decoder Shape
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Circle" ->
            Json.Decode.succeed Circle |>
            Json.Decode.Pipeline.required "contents" Json.Decode.int
        
        "Rectangle" ->
            Json.Decode.succeed Rectangle |>
            Json.Decode.Pipeline.required "contents" Auto.IntVec2.decode
        
        _ ->
            Json.Decode.fail "No matching constructor")