module Auto.Button exposing (..)

import Json.Decode
import Json.Encode


type Button 
    = Blue 
    | Yellow 
    | Red 
    | Green 


encode : Button -> Json.Encode.Value
encode a =
    case a of
        Blue ->
            Json.Encode.string "Blue"
        
        Yellow ->
            Json.Encode.string "Yellow"
        
        Red ->
            Json.Encode.string "Red"
        
        Green ->
            Json.Encode.string "Green"


decode : Json.Decode.Decoder Button
decode =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Blue" ->
            Json.Decode.succeed Blue
        
        "Yellow" ->
            Json.Decode.succeed Yellow
        
        "Red" ->
            Json.Decode.succeed Red
        
        "Green" ->
            Json.Decode.succeed Green
        
        _ ->
            Json.Decode.fail "No matching constructor")