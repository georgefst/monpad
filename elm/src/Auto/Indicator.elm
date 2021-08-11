module Auto.Indicator exposing
    ( Indicator
    , decode
    )

import Auto.Colour
import Auto.Shape
import Json.Decode
import Json.Decode.Pipeline
import Math.Vector2
import Util


type alias Indicator  =
    { hollowness : Float
    , arcStart : Float
    , arcEnd : Float
    , centre : Math.Vector2.Vec2
    , colour : Auto.Colour.Colour
    , shape : Auto.Shape.Shape }


decode : Json.Decode.Decoder Indicator
decode =
    Json.Decode.succeed Indicator |>
    Json.Decode.Pipeline.required "hollowness" Json.Decode.float |>
    Json.Decode.Pipeline.required "arcStart" Json.Decode.float |>
    Json.Decode.Pipeline.required "arcEnd" Json.Decode.float |>
    Json.Decode.Pipeline.required "centre" Util.decodeVec2 |>
    Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "shape" Auto.Shape.decode