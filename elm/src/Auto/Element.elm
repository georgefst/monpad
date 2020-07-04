module Auto.Element exposing (..)

import Auto.Button
import Auto.Colour
import Json.Decode
import Json.Decode.Pipeline


type Element 
    = Stick { radius : Float
    , range : Float
    , stickColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour }
    | Button { button : Auto.Button.Button, colour : Auto.Colour.Colour }


decode : Json.Decode.Decoder Element
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Stick" ->
            Json.Decode.map Stick (Json.Decode.succeed (\b c d e -> { radius = b
            , range = c
            , stickColour = d
            , backgroundColour = e }) |>
            Json.Decode.Pipeline.required "radius" Json.Decode.float |>
            Json.Decode.Pipeline.required "range" Json.Decode.float |>
            Json.Decode.Pipeline.required "stickColour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode)
        
        "Button" ->
            Json.Decode.map Button (Json.Decode.succeed (\b c -> { button = b
            , colour = c }) |>
            Json.Decode.Pipeline.required "button" Auto.Button.decode |>
            Json.Decode.Pipeline.required "colour" Auto.Colour.decode)
        
        _ ->
            Json.Decode.fail "No matching constructor")