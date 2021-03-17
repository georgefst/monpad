module Auto.ResetLayout exposing (..)

import Json.Decode


type ResetLayout 
    = StateReset 
    | FullReset 


decode : Json.Decode.Decoder ResetLayout
decode =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "stateReset" ->
            Json.Decode.succeed StateReset

        "fullReset" ->
            Json.Decode.succeed FullReset

        _ ->
            Json.Decode.fail "No matching constructor")