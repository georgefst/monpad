module Auto.PosX exposing
    ( PosX(..)
    , decode
    )

import Json.Decode


type PosX 
    = Left 
    | Centre 
    | Right 


decode : Json.Decode.Decoder PosX
decode =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Left" ->
            Json.Decode.succeed Left

        "Centre" ->
            Json.Decode.succeed Centre

        "Right" ->
            Json.Decode.succeed Right

        _ ->
            Json.Decode.fail "No matching constructor")