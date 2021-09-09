module Auto.NumberInput exposing
    ( NumberInput
    , decode
    )

import Auto.TextStyle
import Json.Decode
import Json.Decode.Pipeline


type alias NumberInput  =
    { textStyle : Auto.TextStyle.TextStyle
    , min : Maybe Float
    , max : Maybe Float
    , step : Maybe Float }


decode : Json.Decode.Decoder NumberInput
decode =
    Json.Decode.succeed NumberInput |>
    Json.Decode.Pipeline.required "textStyle" Auto.TextStyle.decode |>
    Json.Decode.Pipeline.required "min" (Json.Decode.nullable Json.Decode.float) |>
    Json.Decode.Pipeline.required "max" (Json.Decode.nullable Json.Decode.float) |>
    Json.Decode.Pipeline.required "step" (Json.Decode.nullable Json.Decode.float)