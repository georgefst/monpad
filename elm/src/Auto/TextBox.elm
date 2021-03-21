module Auto.TextBox exposing (..)

import Auto.TextStyle
import Json.Decode
import Json.Decode.Pipeline


type alias TextBox  =
    { text : String, style : Auto.TextStyle.TextStyle }


decode : Json.Decode.Decoder TextBox
decode =
    Json.Decode.succeed TextBox |>
    Json.Decode.Pipeline.required "text" Json.Decode.string |>
    Json.Decode.Pipeline.required "style" Auto.TextStyle.decode