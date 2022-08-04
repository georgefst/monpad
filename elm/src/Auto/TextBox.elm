module Auto.TextBox exposing
    ( TextBox
    , decode
    )

import Auto.PosX
import Auto.PosY
import Auto.TextStyle
import Json.Decode
import Json.Decode.Pipeline


type alias TextBox  =
    { text : String
    , style : Auto.TextStyle.TextStyle
    , alignX : Auto.PosX.PosX
    , alignY : Auto.PosY.PosY }


decode : Json.Decode.Decoder TextBox
decode =
    Json.Decode.succeed TextBox |>
    Json.Decode.Pipeline.required "text" Json.Decode.string |>
    Json.Decode.Pipeline.required "style" Auto.TextStyle.decode |>
    Json.Decode.Pipeline.required "alignX" Auto.PosX.decode |>
    Json.Decode.Pipeline.required "alignY" Auto.PosY.decode