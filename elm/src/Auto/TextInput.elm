module Auto.TextInput exposing
    ( TextInput
    , decode
    )

import Auto.TextStyle
import Json.Decode
import Json.Decode.Pipeline


type alias TextInput  =
    { textStyle : Auto.TextStyle.TextStyle
    , minLength : Maybe Int
    , maxLength : Maybe Int }


decode : Json.Decode.Decoder TextInput
decode =
    Json.Decode.succeed TextInput |>
    Json.Decode.Pipeline.required "textStyle" Auto.TextStyle.decode |>
    Json.Decode.Pipeline.required "minLength" (Json.Decode.nullable Json.Decode.int) |>
    Json.Decode.Pipeline.required "maxLength" (Json.Decode.nullable Json.Decode.int)