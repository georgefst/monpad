module Auto.TextStyle exposing
    ( TextStyle
    , decode
    )

import Auto.Colour
import Auto.TextShadow
import Json.Decode
import Json.Decode.Pipeline


type alias TextStyle  =
    { size : Int
    , colour : Auto.Colour.Colour
    , bold : Bool
    , italic : Bool
    , underline : Bool
    , shadow : List Auto.TextShadow.TextShadow
    , font : String }


decode : Json.Decode.Decoder TextStyle
decode =
    Json.Decode.succeed TextStyle |>
    Json.Decode.Pipeline.required "size" Json.Decode.int |>
    Json.Decode.Pipeline.required "colour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "bold" Json.Decode.bool |>
    Json.Decode.Pipeline.required "italic" Json.Decode.bool |>
    Json.Decode.Pipeline.required "underline" Json.Decode.bool |>
    Json.Decode.Pipeline.required "shadow" (Json.Decode.list Auto.TextShadow.decode) |>
    Json.Decode.Pipeline.required "font" Json.Decode.string