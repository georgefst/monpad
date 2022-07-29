module Auto.Image exposing
    ( Image
    , decode
    )

import Json.Decode
import Json.Decode.Pipeline


type alias Image  =
    { url : String }


decode : Json.Decode.Decoder Image
decode =
    Json.Decode.succeed Image |>
    Json.Decode.Pipeline.required "url" Json.Decode.string