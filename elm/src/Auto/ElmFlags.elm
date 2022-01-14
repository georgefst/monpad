module Auto.ElmFlags exposing
    ( ElmFlags
    , decode
    )

import Auto.Layout
import Json.Decode
import Json.Decode.Pipeline


type alias ElmFlags  =
    { layouts : List Auto.Layout.Layout
    , username : String
    , supportsFullscreen : Bool }


decode : Json.Decode.Decoder ElmFlags
decode =
    Json.Decode.succeed ElmFlags |>
    Json.Decode.Pipeline.required "layouts" (Json.Decode.list Auto.Layout.decode) |>
    Json.Decode.Pipeline.required "username" Json.Decode.string |>
    Json.Decode.Pipeline.required "supportsFullscreen" Json.Decode.bool