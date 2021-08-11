module Auto.FullElement exposing
    ( FullElement
    , decode
    )

import Auto.Element
import Auto.Image
import Auto.IntVec2
import Auto.TextBox
import Json.Decode
import Json.Decode.Pipeline


type alias FullElement  =
    { element : Auto.Element.Element
    , location : Auto.IntVec2.IntVec2
    , name : String
    , text : Maybe Auto.TextBox.TextBox
    , image : Maybe Auto.Image.Image
    , hidden : Bool }


decode : Json.Decode.Decoder FullElement
decode =
    Json.Decode.succeed FullElement |>
    Json.Decode.Pipeline.required "element" Auto.Element.decode |>
    Json.Decode.Pipeline.required "location" Auto.IntVec2.decode |>
    Json.Decode.Pipeline.required "name" Json.Decode.string |>
    Json.Decode.Pipeline.required "text" (Json.Decode.nullable Auto.TextBox.decode) |>
    Json.Decode.Pipeline.required "image" (Json.Decode.nullable Auto.Image.decode) |>
    Json.Decode.Pipeline.required "hidden" Json.Decode.bool