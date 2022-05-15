module Auto.Encoding exposing
    ( Encoding(..)
    , decode
    )

import Json.Decode


type Encoding 
    = JSONEncoding 
    | BinaryEncoding 


decode : Json.Decode.Decoder Encoding
decode =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "JSONEncoding" ->
            Json.Decode.succeed JSONEncoding

        "BinaryEncoding" ->
            Json.Decode.succeed BinaryEncoding

        _ ->
            Json.Decode.fail "No matching constructor")