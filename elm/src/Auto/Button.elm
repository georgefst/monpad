module Auto.Button exposing
    ( Button
    , decode
    )

import Auto.Colour
import Auto.Shape
import Json.Decode
import Json.Decode.Pipeline


type alias Button  =
    { shape : Auto.Shape.Shape, colour : Auto.Colour.Colour, buttonData : () }


decode : Json.Decode.Decoder Button
decode =
    Json.Decode.succeed Button |>
    Json.Decode.Pipeline.required "shape" Auto.Shape.decode |>
    Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "buttonData" (Json.Decode.succeed ())