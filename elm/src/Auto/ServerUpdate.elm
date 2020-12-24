module Auto.ServerUpdate exposing (..)

import Json.Decode
import Json.Decode.Pipeline


type ServerUpdate 
    = SetImageURL String String


decode : Json.Decode.Decoder ServerUpdate
decode =
    Json.Decode.succeed SetImageURL |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.string)