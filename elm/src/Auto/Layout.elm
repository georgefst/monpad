module Auto.Layout exposing (..)

import Auto.FullElement
import Json.Decode
import Json.Decode.Pipeline
import Util.IntVector2


type alias Layout  =
    { elements : List Auto.FullElement.FullElement
    , grid : Util.IntVector2.IntVector2 }


decode : Json.Decode.Decoder Layout
decode =
    Json.Decode.succeed Layout |>
    Json.Decode.Pipeline.required "elements" (Json.Decode.list Auto.FullElement.decode) |>
    Json.Decode.Pipeline.required "grid" Util.IntVector2.decode