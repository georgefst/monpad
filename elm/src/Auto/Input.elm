module Auto.Input exposing
    ( Input
    , decode
    )

import Auto.InputType
import Json.Decode
import Json.Decode.Pipeline


type alias Input  =
    { width : Int, height : Int, inputType : Auto.InputType.InputType }


decode : Json.Decode.Decoder Input
decode =
    Json.Decode.succeed Input |>
    Json.Decode.Pipeline.required "width" Json.Decode.int |>
    Json.Decode.Pipeline.required "height" Json.Decode.int |>
    Json.Decode.Pipeline.required "inputType" Auto.InputType.decode