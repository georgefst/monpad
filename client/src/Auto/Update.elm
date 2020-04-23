module Auto.Update exposing (..)

import Auto.Button
import Json.Decode
import Json.Decode.Pipeline
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


decode : Json.Decode.Decoder Update
decode =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "ButtonUp" ->
            Json.Decode.succeed ButtonUp |>
            Json.Decode.Pipeline.required "contents" Auto.Button.decode

        "ButtonDown" ->
            Json.Decode.succeed ButtonDown |>
            Json.Decode.Pipeline.required "contents" Auto.Button.decode

        "Stick" ->
            Json.Decode.succeed Stick |>
            Json.Decode.Pipeline.required "contents" Util.decodeVec2

        _ ->
            Json.Decode.fail "No matching constructor")
