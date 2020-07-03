module Auto.ElmFlags exposing (..)

import Auto.Layout
import Json.Decode
import Json.Decode.Pipeline


type alias ElmFlags  =
    { layout : Auto.Layout.Layout, username : String }


decode : Json.Decode.Decoder ElmFlags
decode =
    Json.Decode.succeed ElmFlags |>
    Json.Decode.Pipeline.required "layout" Auto.Layout.decode |>
    Json.Decode.Pipeline.required "username" Json.Decode.string