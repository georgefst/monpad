module Auto.Image exposing (..)

import Json.Decode
import Json.Decode.Pipeline


type alias Image  =
    { width : Int, height : Int, url : String }


decode : Json.Decode.Decoder Image
decode =
    Json.Decode.succeed Image |>
    Json.Decode.Pipeline.required "width" Json.Decode.int |>
    Json.Decode.Pipeline.required "height" Json.Decode.int |>
    Json.Decode.Pipeline.required "url" Json.Decode.string