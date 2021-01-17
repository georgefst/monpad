module Auto.ServerUpdate exposing (..)

import Auto.FullElement
import Auto.Layout
import Json.Decode
import Json.Decode.Pipeline


type ServerUpdate 
    = SetImageURL String String
    | SetLayout Auto.Layout.Layout
    | AddElement Auto.FullElement.FullElement
    | RemoveElement String


decode : Json.Decode.Decoder ServerUpdate
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "SetImageURL" ->
            Json.Decode.field "contents" (Json.Decode.succeed SetImageURL |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.string))
        
        "SetLayout" ->
            Json.Decode.succeed SetLayout |>
            Json.Decode.Pipeline.required "contents" Auto.Layout.decode
        
        "AddElement" ->
            Json.Decode.succeed AddElement |>
            Json.Decode.Pipeline.required "contents" Auto.FullElement.decode
        
        "RemoveElement" ->
            Json.Decode.succeed RemoveElement |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string
        
        _ ->
            Json.Decode.fail "No matching constructor")