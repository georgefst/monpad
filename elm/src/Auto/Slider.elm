module Auto.Slider exposing (..)

import Auto.Colour
import Auto.IntVec2
import Json.Decode
import Json.Decode.Pipeline


type alias Slider  =
    { radius : Int
    , offset : Auto.IntVec2.IntVec2
    , width : Int
    , initialPosition : Float
    , resetOnRelease : Bool
    , sliderColour : Auto.Colour.Colour
    , backgroundColour : Auto.Colour.Colour
    , sliderData : () }


decode : Json.Decode.Decoder Slider
decode =
    Json.Decode.succeed Slider |>
    Json.Decode.Pipeline.required "radius" Json.Decode.int |>
    Json.Decode.Pipeline.required "offset" Auto.IntVec2.decode |>
    Json.Decode.Pipeline.required "width" Json.Decode.int |>
    Json.Decode.Pipeline.required "initialPosition" Json.Decode.float |>
    Json.Decode.Pipeline.required "resetOnRelease" Json.Decode.bool |>
    Json.Decode.Pipeline.required "sliderColour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "sliderData" (Json.Decode.succeed ())