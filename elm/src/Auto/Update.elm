module Auto.Update exposing (..)

import Auto.Button
import Json.Encode
import Math.Vector2
import Util


type Update 
    = ButtonUp Auto.Button.Button
    | ButtonDown Auto.Button.Button
    | Stick Math.Vector2.Vec2


encode : Update -> Json.Encode.Value
encode a =
    case a of
        ButtonUp b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ButtonUp")
            , ("contents" , Auto.Button.encode b) ]
        
        ButtonDown b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ButtonDown")
            , ("contents" , Auto.Button.encode b) ]
        
        Stick b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "Stick")
            , ("contents" , Util.encodeVec2 b) ]