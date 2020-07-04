module Auto.FullElement exposing (..)

import Auto.Element
import Json.Decode
import Json.Decode.Pipeline
import Math.Vector2
import Util


type alias FullElement  =
    { element : Auto.Element.Element
    , location : Math.Vector2.Vec2
    , name : String }


decode : Json.Decode.Decoder FullElement
decode =
    Json.Decode.succeed FullElement |>
    Json.Decode.Pipeline.required "element" Auto.Element.decode |>
    Json.Decode.Pipeline.required "location" Util.decodeVec2 |>
    Json.Decode.Pipeline.required "name" Json.Decode.string