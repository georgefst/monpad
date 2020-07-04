module Auto.Layout exposing (..)

import Auto.FullElement
import Json.Decode
import Json.Decode.Pipeline


type alias Layout  =
    { elements : List Auto.FullElement.FullElement }


decode : Json.Decode.Decoder Layout
decode =
    Json.Decode.succeed Layout |>
    Json.Decode.Pipeline.required "elements" (Json.Decode.list Auto.FullElement.decode)