module Auto.ResetLayout exposing (..)

import Json.Decode


type ResetLayout 
    = StateReset 
    | FullReset 


decode : Json.Decode.Decoder ResetLayout
decode =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "StateReset" ->
            Json.Decode.succeed StateReset

        "FullReset" ->
            Json.Decode.succeed FullReset

        _ ->
            Json.Decode.fail "No matching constructor")