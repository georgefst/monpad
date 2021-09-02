module Auto.InputType exposing
    ( InputType(..)
    , decode
    )

import Auto.TextStyle
import Json.Decode
import Json.Decode.Pipeline


type InputType 
    = CheckBox 
    | Number Auto.TextStyle.TextStyle
    | Text Auto.TextStyle.TextStyle


decode : Json.Decode.Decoder InputType
decode =
    Json.Decode.oneOf [ Json.Decode.succeed CheckBox
    , Json.Decode.succeed Number |>
    Json.Decode.Pipeline.required "Number" Auto.TextStyle.decode
    , Json.Decode.succeed Text |>
    Json.Decode.Pipeline.required "Text" Auto.TextStyle.decode ]