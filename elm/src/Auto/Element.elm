module Auto.Element exposing (..)

import Auto.Colour
import Auto.Shape
import Json.Decode
import Json.Decode.Pipeline


type Element 
    = Stick { radius : Int
    , range : Int
    , stickColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour
    , stickDataX : ()
    , stickDataY : () }
    | Button { shape : Auto.Shape.Shape
    , colour : Auto.Colour.Colour
    , buttonData : () }
    | Slider { radius : Int
    , length : Int
    , width : Int
    , sliderColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour
    , vertical : Bool
    , sliderData : () }


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
            Json.Decode.Pipeline.required "stickDataX" (Json.Decode.succeed ()) |>
            Json.Decode.Pipeline.required "stickDataY" (Json.Decode.succeed ()))
        
        "Button" ->
            Json.Decode.map Button (Json.Decode.succeed (\b c d -> { shape = b
            , colour = c
            , buttonData = d }) |>
            Json.Decode.Pipeline.required "shape" Auto.Shape.decode |>
            Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "buttonData" (Json.Decode.succeed ()))
        
        "Slider" ->
            Json.Decode.map Slider (Json.Decode.succeed (\b c d e f g h -> { radius = b
            , length = c
            , width = d
            , sliderColour = e
            , backgroundColour = f
            , vertical = g
            , sliderData = h }) |>
            Json.Decode.Pipeline.required "radius" Json.Decode.int |>
            Json.Decode.Pipeline.required "length" Json.Decode.int |>
            Json.Decode.Pipeline.required "width" Json.Decode.int |>
            Json.Decode.Pipeline.required "sliderColour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "vertical" Json.Decode.bool |>
            Json.Decode.Pipeline.required "sliderData" (Json.Decode.succeed ()))
        
        _ ->
            Json.Decode.fail "No matching constructor")