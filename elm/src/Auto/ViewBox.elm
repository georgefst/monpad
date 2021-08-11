module Auto.ViewBox exposing
    ( ViewBox
    , decode
    )

import Json.Decode
import Json.Decode.Pipeline


type alias ViewBox  =
    { x : Int, y : Int, w : Int, h : Int }


decode : Json.Decode.Decoder ViewBox
decode =
    Json.Decode.succeed ViewBox |>
    Json.Decode.Pipeline.required "x" Json.Decode.int |>
    Json.Decode.Pipeline.required "y" Json.Decode.int |>
    Json.Decode.Pipeline.required "w" Json.Decode.int |>
    Json.Decode.Pipeline.required "h" Json.Decode.int