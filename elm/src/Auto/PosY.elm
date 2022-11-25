module Auto.PosY exposing
    ( PosY(..)
    , decode
    )

import Json.Decode


type PosY 
    = Top 
    | Middle 
    | Bottom 


decode : Json.Decode.Decoder PosY
decode =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Top" ->
            Json.Decode.succeed Top

        "Middle" ->
            Json.Decode.succeed Middle

        "Bottom" ->
            Json.Decode.succeed Bottom

        _ ->
            Json.Decode.fail "No matching constructor")