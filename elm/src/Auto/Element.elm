module Auto.Element exposing (..)

import Auto.Button
import Auto.Colour
import Auto.Unit
import Json.Decode
import Json.Decode.Pipeline


type Element 
    = Stick { radius : Int
    , range : Int
    , stickColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour
    , stickDataX : Auto.Unit.Unit
    , stickDataY : Auto.Unit.Unit }
    | Button { button : Auto.Button.Button
    , colour : Auto.Colour.Colour
    , buttonData : Auto.Unit.Unit }


decode : Json.Decode.Decoder Element
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Stick" ->
            Json.Decode.map Stick (Json.Decode.succeed (\b c d e f g -> { radius = b
            , range = c
            , stickColour = d
            , backgroundColour = e
            , stickDataX = f
            , stickDataY = g }) |>
            Json.Decode.Pipeline.required "radius" Json.Decode.int |>
            Json.Decode.Pipeline.required "range" Json.Decode.int |>
            Json.Decode.Pipeline.required "stickColour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "stickDataX" Auto.Unit.decode |>
            Json.Decode.Pipeline.required "stickDataY" Auto.Unit.decode)
        
        "Button" ->
            Json.Decode.map Button (Json.Decode.succeed (\b c d -> { button = b
            , colour = c
            , buttonData = d }) |>
            Json.Decode.Pipeline.required "button" Auto.Button.decode |>
            Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "buttonData" Auto.Unit.decode)
        
        _ ->
            Json.Decode.fail "No matching constructor")