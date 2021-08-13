module Auto.InputType exposing
    ( InputType(..)
    , decode
    )

import Json.Decode


type InputType 
    = CheckBox 
    | Number 
    | Text 


decode : Json.Decode.Decoder InputType
decode =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "CheckBox" ->
            Json.Decode.succeed CheckBox

        "Number" ->
            Json.Decode.succeed Number

        "Text" ->
            Json.Decode.succeed Text

        _ ->
            Json.Decode.fail "No matching constructor")