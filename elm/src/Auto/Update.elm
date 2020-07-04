module Auto.Update exposing (..)

import Json.Encode
import Math.Vector2
import Util


type Update 
    = ButtonUp String
    | ButtonDown String
    | StickMove String Math.Vector2.Vec2


encode : Update -> Json.Encode.Value
encode a =
    case a of
        ButtonUp b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ButtonUp")
            , ("contents" , Json.Encode.string b) ]
        
        ButtonDown b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ButtonDown")
            , ("contents" , Json.Encode.string b) ]
        
        StickMove b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "StickMove")
            , ("contents" , Json.Encode.list identity [ Json.Encode.string b
            , Util.encodeVec2 c ]) ]