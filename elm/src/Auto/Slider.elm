module Auto.Slider exposing (..)

import Auto.Colour
import Json.Decode
import Json.Decode.Pipeline


type alias Slider  =
    { radius : Int
    , length : Int
    , width : Int
    , sliderColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour
    , vertical : Bool
    , sliderData : () }


decode : Json.Decode.Decoder Slider
decode =
    Json.Decode.succeed Slider |>
    Json.Decode.Pipeline.required "radius" Json.Decode.int |>
    Json.Decode.Pipeline.required "length" Json.Decode.int |>
    Json.Decode.Pipeline.required "width" Json.Decode.int |>
    Json.Decode.Pipeline.required "sliderColour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "vertical" Json.Decode.bool |>
    Json.Decode.Pipeline.required "sliderData" (Json.Decode.succeed ())