module Auto.Element exposing (..)

import Auto.Colour
import Auto.Shape
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
    | Button { shape : Auto.Shape.Shape
    , colour : Auto.Colour.Colour
    , buttonData : Auto.Unit.Unit }
    | Slider { radius : Int
    , rangeX : Int
    , rangeY : Int
    , sliderColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour
    , sliderData : Auto.Unit.Unit }


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
            Json.Decode.map Button (Json.Decode.succeed (\b c d -> { shape = b
            , colour = c
            , buttonData = d }) |>
            Json.Decode.Pipeline.required "shape" Auto.Shape.decode |>
            Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "buttonData" Auto.Unit.decode)
        
        "Slider" ->
            Json.Decode.map Slider (Json.Decode.succeed (\b c d e f g -> { radius = b
            , rangeX = c
            , rangeY = d
            , sliderColour = e
            , backgroundColour = f
            , sliderData = g }) |>
            Json.Decode.Pipeline.required "radius" Json.Decode.int |>
            Json.Decode.Pipeline.required "rangeX" Json.Decode.int |>
            Json.Decode.Pipeline.required "rangeY" Json.Decode.int |>
            Json.Decode.Pipeline.required "sliderColour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode |>
            Json.Decode.Pipeline.required "sliderData" Auto.Unit.decode)
        
        _ ->
            Json.Decode.fail "No matching constructor")