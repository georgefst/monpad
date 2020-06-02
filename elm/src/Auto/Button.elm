module Auto.Button exposing (..)

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