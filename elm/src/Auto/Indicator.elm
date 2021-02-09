module Auto.Indicator exposing (..)

import Auto.Colour
import Json.Decode
import Json.Decode.Pipeline


type alias Indicator  =
    { hollowness : Float
    , arcStart : Float
    , arcEnd : Float
    , colour : Auto.Colour.Colour
    , radius : Int }


decode : Json.Decode.Decoder Indicator
decode =
    Json.Decode.succeed Indicator |>
    Json.Decode.Pipeline.required "hollowness" Json.Decode.float |>
    Json.Decode.Pipeline.required "arcStart" Json.Decode.float |>
    Json.Decode.Pipeline.required "arcEnd" Json.Decode.float |>
    Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "radius" Json.Decode.int
