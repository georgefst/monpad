module Auto.Stick exposing (..)

import Auto.Colour
import Json.Decode
import Json.Decode.Pipeline


type alias Stick  =
    { radius : Float
    , range : Float
    , colour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour }


decode : Json.Decode.Decoder Stick
decode =
    Json.Decode.succeed Stick |>
    Json.Decode.Pipeline.required "radius" Json.Decode.float |>
    Json.Decode.Pipeline.required "range" Json.Decode.float |>
    Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode