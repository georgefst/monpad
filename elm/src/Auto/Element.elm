module Auto.Element exposing (..)

import Auto.Button
import Auto.Colour
import Auto.Stick
import Json.Decode
import Json.Decode.Pipeline


type Element 
    = StickElement Auto.Stick.Stick
    | ButtonElement Auto.Button.Button Auto.Colour.Colour


decode : Json.Decode.Decoder Element
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "StickElement" ->
            Json.Decode.succeed StickElement |>
            Json.Decode.Pipeline.required "contents" Auto.Stick.decode
        
        "ButtonElement" ->
            Json.Decode.field "contents" (Json.Decode.succeed ButtonElement |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 Auto.Button.decode) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 Auto.Colour.decode))
        
        _ ->
            Json.Decode.fail "No matching constructor")