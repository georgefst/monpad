module Auto.Update exposing (..)

import Json.Encode
import Math.Vector2
import Util


type Update 
    = ButtonUp String
    | ButtonDown String
    | StickMove String Math.Vector2.Vec2
    | SliderMove String Float


encode : Update -> Json.Encode.Value
encode a =
    case a of
        ButtonUp b ->
            Json.Encode.object [("buttonUp" , Json.Encode.string b)]

        ButtonDown b ->
            Json.Encode.object [("buttonDown" , Json.Encode.string b)]

        StickMove b c ->
            Json.Encode.object [ ("stickMove" , Json.Encode.list identity [ Json.Encode.string b
            , Util.encodeVec2 c ]) ]

        SliderMove b c ->
            Json.Encode.object [ ("sliderMove" , Json.Encode.list identity [ Json.Encode.string b
            , Json.Encode.float c ]) ]