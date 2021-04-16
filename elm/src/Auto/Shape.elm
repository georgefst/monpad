module Auto.Shape exposing (..)

import Auto.IntVec2
import Json.Decode
import Json.Decode.Pipeline


type Shape 
    = Circle Int
    | Rectangle Auto.IntVec2.IntVec2


decode : Json.Decode.Decoder Shape
decode =
    Json.Decode.oneOf [ Json.Decode.succeed Circle |>
    Json.Decode.Pipeline.required "Circle" Json.Decode.int
    , Json.Decode.succeed Rectangle |>
    Json.Decode.Pipeline.required "Rectangle" Auto.IntVec2.decode ]