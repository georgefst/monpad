module Auto.FullElement exposing (..)

import Auto.Element
import Auto.IntVec2
import Json.Decode
import Json.Decode.Pipeline


type alias FullElement  =
    { element : Auto.Element.Element
    , location : Auto.IntVec2.IntVec2
    , name : String
    , showName : Bool }


decode : Json.Decode.Decoder FullElement
decode =
    Json.Decode.succeed FullElement |>
    Json.Decode.Pipeline.required "element" Auto.Element.decode |>
    Json.Decode.Pipeline.required "location" Auto.IntVec2.decode |>
    Json.Decode.Pipeline.required "name" Json.Decode.string |>
    Json.Decode.Pipeline.required "showName" Json.Decode.bool