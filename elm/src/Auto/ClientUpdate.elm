module Auto.ClientUpdate exposing
    ( ClientUpdate(..)
    , encode
    )

import Json.Encode
import Math.Vector2
import Util


type ClientUpdate 
    = ButtonUp String
    | ButtonDown String
    | StickMove String Math.Vector2.Vec2
    | SliderMove String Float
    | InputBool String Bool
    | InputNumber String Int
    | InputText String String
    | SubmitInput String
    | Pong String


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
            , Util.encodeFloatRounded c ]) ]

        InputBool b c ->
            Json.Encode.object [ ("InputBool" , Json.Encode.list identity [ Json.Encode.string b
            , Json.Encode.bool c ]) ]

        InputNumber b c ->
            Json.Encode.object [ ("InputNumber" , Json.Encode.list identity [ Json.Encode.string b
            , Json.Encode.int c ]) ]

        InputText b c ->
            Json.Encode.object [ ("InputText" , Json.Encode.list identity [ Json.Encode.string b
            , Json.Encode.string c ]) ]

        SubmitInput b ->
            Json.Encode.object [("SubmitInput" , Json.Encode.string b)]

        Pong b ->
            Json.Encode.object [("Pong" , Json.Encode.string b)]