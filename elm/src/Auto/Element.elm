module Auto.Element exposing
    ( Element(..)
    , decode
    )

import Auto.Button
import Auto.Indicator
import Auto.Slider
import Auto.Stick
import Json.Decode
import Json.Decode.Pipeline


type Element 
    = Stick Auto.Stick.Stick
    | Button Auto.Button.Button
    | Slider Auto.Slider.Slider
    | Indicator Auto.Indicator.Indicator
    | Empty 


decode : Json.Decode.Decoder Element
decode =
    Json.Decode.oneOf [ Json.Decode.succeed Stick |>
    Json.Decode.Pipeline.required "Stick" Auto.Stick.decode
    , Json.Decode.succeed Button |>
    Json.Decode.Pipeline.required "Button" Auto.Button.decode
    , Json.Decode.succeed Slider |>
    Json.Decode.Pipeline.required "Slider" Auto.Slider.decode
    , Json.Decode.succeed Indicator |>
    Json.Decode.Pipeline.required "Indicator" Auto.Indicator.decode
    , Json.Decode.succeed Empty ]