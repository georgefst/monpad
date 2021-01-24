module Auto.Element exposing (..)

import Auto.Button
import Auto.Image
import Auto.Slider
import Auto.Stick
import Json.Decode
import Json.Decode.Pipeline


type Element 
    = Stick Auto.Stick.Stick
    | Button Auto.Button.Button
    | Slider Auto.Slider.Slider
    | Image Auto.Image.Image


decode : Json.Decode.Decoder Element
decode =
    Json.Decode.oneOf [ Json.Decode.succeed Stick |>
    Json.Decode.Pipeline.required "stick" Auto.Stick.decode
    , Json.Decode.succeed Button |>
    Json.Decode.Pipeline.required "button" Auto.Button.decode
    , Json.Decode.succeed Slider |>
    Json.Decode.Pipeline.required "slider" Auto.Slider.decode
    , Json.Decode.succeed Image |>
    Json.Decode.Pipeline.required "image" Auto.Image.decode ]