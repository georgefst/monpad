module Auto.IntVec2 exposing (..)

import Json.Decode
import Json.Decode.Pipeline


type alias IntVec2  =
    { x : Int, y : Int }


decode : Json.Decode.Decoder IntVec2
decode =
    Json.Decode.succeed IntVec2 |>
    Json.Decode.Pipeline.required "x" Json.Decode.int |>
    Json.Decode.Pipeline.required "y" Json.Decode.int