module Auto.Stick exposing (..)

import Auto.Colour
import Json.Decode
import Json.Decode.Pipeline


type alias Stick  =
    { radius : Int
    , range : Int
    , stickColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour
    , stickDataX : ()
    , stickDataY : () }


decode : Json.Decode.Decoder Stick
decode =
    Json.Decode.succeed Stick |>
    Json.Decode.Pipeline.required "radius" Json.Decode.int |>
    Json.Decode.Pipeline.required "range" Json.Decode.int |>
    Json.Decode.Pipeline.required "stickColour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "stickDataX" (Json.Decode.succeed ()) |>
    Json.Decode.Pipeline.required "stickDataY" (Json.Decode.succeed ())