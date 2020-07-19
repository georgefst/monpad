module Auto.Layout exposing (..)

import Auto.FullElement
import Auto.IntVec2
import Json.Decode
import Json.Decode.Pipeline


type alias Layout  =
    { elements : List Auto.FullElement.FullElement
    , grid : Auto.IntVec2.IntVec2 }


decode : Json.Decode.Decoder Layout
decode =
    Json.Decode.succeed Layout |>
    Json.Decode.Pipeline.required "elements" (Json.Decode.list Auto.FullElement.decode) |>
    Json.Decode.Pipeline.required "grid" Auto.IntVec2.decode