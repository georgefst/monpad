module Auto.TextShadow exposing (..)

import Auto.Colour
import Auto.IntVec2
import Json.Decode
import Json.Decode.Pipeline


type alias TextShadow  =
    { offset : Auto.IntVec2.IntVec2, blur : Int, colour : Auto.Colour.Colour }


decode : Json.Decode.Decoder TextShadow
decode =
    Json.Decode.succeed TextShadow |>
    Json.Decode.Pipeline.required "offset" Auto.IntVec2.decode |>
    Json.Decode.Pipeline.required "blur" Json.Decode.int |>
    Json.Decode.Pipeline.required "colour" Auto.Colour.decode