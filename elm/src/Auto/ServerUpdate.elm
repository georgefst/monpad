module Auto.ServerUpdate exposing (..)

import Json.Decode
import Json.Decode.Pipeline


type ServerUpdate 
    = SetImageURL String String
    | ServerUpdatePlaceholder 


decode : Json.Decode.Decoder ServerUpdate
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "SetImageURL" ->
            Json.Decode.field "contents" (Json.Decode.succeed SetImageURL |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.string))
        
        "ServerUpdatePlaceholder" ->
            Json.Decode.succeed ServerUpdatePlaceholder
        
        _ ->
            Json.Decode.fail "No matching constructor")