module Auto.Colour exposing (..)

import Json.Decode
import Json.Decode.Pipeline


type alias Colour  =
    { red : Float, green : Float, blue : Float, alpha : Float }


decode : Json.Decode.Decoder Colour
decode =
    Json.Decode.succeed Colour |>
    Json.Decode.Pipeline.required "red" Json.Decode.float |>
    Json.Decode.Pipeline.required "green" Json.Decode.float |>
    Json.Decode.Pipeline.required "blue" Json.Decode.float |>
    Json.Decode.Pipeline.required "alpha" Json.Decode.float