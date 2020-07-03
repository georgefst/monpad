module Auto.Layout exposing (..)

import Auto.Colour
import Json.Decode
import Json.Decode.Pipeline


type alias Layout  =
    { colourBlue : Auto.Colour.Colour
    , colourYellow : Auto.Colour.Colour
    , colourRed : Auto.Colour.Colour
    , colourGreen : Auto.Colour.Colour }


decode : Json.Decode.Decoder Layout
decode =
    Json.Decode.succeed Layout |>
    Json.Decode.Pipeline.required "colourBlue" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "colourYellow" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "colourRed" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "colourGreen" Auto.Colour.decode