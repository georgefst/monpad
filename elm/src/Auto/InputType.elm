module Auto.InputType exposing
    ( InputType(..)
    , decode
    )

import Auto.NumberInput
import Auto.TextInput
import Json.Decode
import Json.Decode.Pipeline


type InputType 
    = CheckBox ()
    | Number Auto.NumberInput.NumberInput
    | Text Auto.TextInput.TextInput


decode : Json.Decode.Decoder InputType
decode =
    Json.Decode.oneOf [ Json.Decode.succeed CheckBox |>
    Json.Decode.Pipeline.required "CheckBox" (Json.Decode.succeed ())
    , Json.Decode.succeed Number |>
    Json.Decode.Pipeline.required "Number" Auto.NumberInput.decode
    , Json.Decode.succeed Text |>
    Json.Decode.Pipeline.required "Text" Auto.TextInput.decode ]