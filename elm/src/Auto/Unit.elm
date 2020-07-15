module Auto.Unit exposing (..)

import Json.Decode
import Json.Encode


type Unit 
    = Unit 


encode : Unit -> Json.Encode.Value
encode a =
    case a of
        Unit ->
            Json.Encode.list identity []


decode : Json.Decode.Decoder Unit
decode =
    Json.Decode.succeed Unit