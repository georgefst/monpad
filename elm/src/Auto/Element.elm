module Auto.Element exposing (..)

import Auto.Button
import Auto.Slider
import Auto.Stick
import Json.Decode
import Json.Decode.Pipeline


type Element 
    = Stick Auto.Stick.Stick
    | Button Auto.Button.Button
    | Slider Auto.Slider.Slider


decode : Json.Decode.Decoder Element
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Stick" ->
            Json.Decode.succeed Stick |>
            Json.Decode.Pipeline.required "contents" Auto.Stick.decode
        
        "Button" ->
            Json.Decode.succeed Button |>
            Json.Decode.Pipeline.required "contents" Auto.Button.decode
        
        "Slider" ->
            Json.Decode.succeed Slider |>
            Json.Decode.Pipeline.required "contents" Auto.Slider.decode
        
        _ ->
            Json.Decode.fail "No matching constructor")