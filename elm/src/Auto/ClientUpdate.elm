module Auto.ClientUpdate exposing (..)

import Json.Encode
import Math.Vector2
import Util


type ClientUpdate 
    = ButtonUp String
    | ButtonDown String
    | StickMove String Math.Vector2.Vec2
    | SliderMove String Float


encode : ClientUpdate -> Json.Encode.Value
encode a =
    case a of
        ButtonUp b ->
            Json.Encode.object [("ButtonUp" , Json.Encode.string b)]

        ButtonDown b ->
            Json.Encode.object [("ButtonDown" , Json.Encode.string b)]

        StickMove b c ->
            Json.Encode.object [ ("StickMove" , Json.Encode.list identity [ Json.Encode.string b
            , Util.encodeVec2 c ]) ]

        SliderMove b c ->
            Json.Encode.object [ ("SliderMove" , Json.Encode.list identity [ Json.Encode.string b
            , Json.Encode.float c ]) ]