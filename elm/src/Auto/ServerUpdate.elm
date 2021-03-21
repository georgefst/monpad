module Auto.ServerUpdate exposing (..)

import Auto.Colour
import Auto.FullElement
import Auto.Layout
import Auto.ResetLayout
import Auto.Shape
import Json.Decode
import Json.Decode.Pipeline


type ServerUpdate 
    = SetImageURL String String
    | PlayAudioURL String
    | Vibrate (List Int)
    | SetText String String
    | SetLayout Auto.Layout.Layout
    | SwitchLayout String
    | AddElement Auto.FullElement.FullElement
    | RemoveElement String
    | SetBackgroundColour Auto.Colour.Colour
    | SetIndicatorHollowness String Float
    | SetIndicatorArcStart String Float
    | SetIndicatorArcEnd String Float
    | SetIndicatorShape String Auto.Shape.Shape
    | SetSliderPosition String Float
    | ResetLayout Auto.ResetLayout.ResetLayout


decode : Json.Decode.Decoder ServerUpdate
decode =
    Json.Decode.oneOf [ Json.Decode.field "setImageURL" (Json.Decode.succeed SetImageURL |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.string))
    , Json.Decode.succeed PlayAudioURL |>
    Json.Decode.Pipeline.required "playAudioURL" Json.Decode.string
    , Json.Decode.succeed Vibrate |>
    Json.Decode.Pipeline.required "vibrate" (Json.Decode.list Json.Decode.int)
    , Json.Decode.field "setText" (Json.Decode.succeed SetText |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.string))
    , Json.Decode.succeed SetLayout |>
    Json.Decode.Pipeline.required "setLayout" Auto.Layout.decode
    , Json.Decode.succeed SwitchLayout |>
    Json.Decode.Pipeline.required "switchLayout" Json.Decode.string
    , Json.Decode.succeed AddElement |>
    Json.Decode.Pipeline.required "addElement" Auto.FullElement.decode
    , Json.Decode.succeed RemoveElement |>
    Json.Decode.Pipeline.required "removeElement" Json.Decode.string
    , Json.Decode.succeed SetBackgroundColour |>
    Json.Decode.Pipeline.required "setBackgroundColour" Auto.Colour.decode
    , Json.Decode.field "setIndicatorHollowness" (Json.Decode.succeed SetIndicatorHollowness |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.field "setIndicatorArcStart" (Json.Decode.succeed SetIndicatorArcStart |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.field "setIndicatorArcEnd" (Json.Decode.succeed SetIndicatorArcEnd |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.field "setIndicatorShape" (Json.Decode.succeed SetIndicatorShape |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Auto.Shape.decode))
    , Json.Decode.field "setSliderPosition" (Json.Decode.succeed SetSliderPosition |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.succeed ResetLayout |>
    Json.Decode.Pipeline.required "resetLayout" Auto.ResetLayout.decode ]