module Auto.Button exposing (..)

import Json.Decode
import Json.Decode.Pipeline
import Util.IntVector2


type Button 
    = Circle Float
    | Rectangle Util.IntVector2.IntVector2


decode : Json.Decode.Decoder Button
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Circle" ->
            Json.Decode.succeed Circle |>
            Json.Decode.Pipeline.required "contents" Json.Decode.float
        
        "Rectangle" ->
            Json.Decode.succeed Rectangle |>
            Json.Decode.Pipeline.required "contents" Util.IntVector2.decode
        
        _ ->
            Json.Decode.fail "No matching constructor")