module Auto.Layout exposing
    ( Layout
    , decode
    )

import Auto.Colour
import Auto.FullElement
import Auto.ViewBox
import Json.Decode
import Json.Decode.Pipeline


type alias Layout  =
    { elements : List Auto.FullElement.FullElement
    , viewBox : Auto.ViewBox.ViewBox
    , backgroundColour : Auto.Colour.Colour
    , name : String }


decode : Json.Decode.Decoder Layout
decode =
    Json.Decode.succeed Layout |>
    Json.Decode.Pipeline.required "elements" (Json.Decode.list Auto.FullElement.decode) |>
    Json.Decode.Pipeline.required "viewBox" Auto.ViewBox.decode |>
    Json.Decode.Pipeline.required "backgroundColour" Auto.Colour.decode |>
    Json.Decode.Pipeline.required "name" Json.Decode.string