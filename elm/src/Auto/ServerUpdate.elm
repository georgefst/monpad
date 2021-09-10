module Auto.ServerUpdate exposing
    ( ServerUpdate(..)
    , decode
    )

import Auto.Colour
import Auto.FullElement
import Auto.Image
import Auto.Layout
import Auto.ResetLayout
import Auto.Shape
import Auto.TextBox
import Json.Decode
import Json.Decode.Pipeline
import Math.Vector2
import Util


type ServerUpdate 
    = PlayAudioURL String
    | Vibrate (List Int)
    | SetImageURL String String
    | AddImage String Auto.Image.Image
    | DeleteImage String
    | SetText String String
    | AddText String Auto.TextBox.TextBox
    | DeleteText String
    | SetLayout Auto.Layout.Layout
    | SwitchLayout String
    | HideElement String
    | ShowElement String
    | AddElement Auto.FullElement.FullElement
    | RemoveElement String
    | SetBackgroundColour Auto.Colour.Colour
    | SetIndicatorHollowness String Float
    | SetIndicatorArcStart String Float
    | SetIndicatorArcEnd String Float
    | SetIndicatorShape String Auto.Shape.Shape
    | SetIndicatorCentre String Math.Vector2.Vec2
    | SetIndicatorColour String Auto.Colour.Colour
    | SetSliderPosition String Float
    | SetButtonColour String Auto.Colour.Colour
    | SetButtonPressed String Bool
    | ResetLayout Auto.ResetLayout.ResetLayout
    | Ping String


decode : Json.Decode.Decoder ServerUpdate
decode =
    Json.Decode.oneOf [ Json.Decode.succeed PlayAudioURL |>
    Json.Decode.Pipeline.required "PlayAudioURL" Json.Decode.string
    , Json.Decode.succeed Vibrate |>
    Json.Decode.Pipeline.required "Vibrate" (Json.Decode.list Json.Decode.int)
    , Json.Decode.field "SetImageURL" (Json.Decode.succeed SetImageURL |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.string))
    , Json.Decode.field "AddImage" (Json.Decode.succeed AddImage |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Auto.Image.decode))
    , Json.Decode.succeed DeleteImage |>
    Json.Decode.Pipeline.required "DeleteImage" Json.Decode.string
    , Json.Decode.field "SetText" (Json.Decode.succeed SetText |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.string))
    , Json.Decode.field "AddText" (Json.Decode.succeed AddText |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Auto.TextBox.decode))
    , Json.Decode.succeed DeleteText |>
    Json.Decode.Pipeline.required "DeleteText" Json.Decode.string
    , Json.Decode.succeed SetLayout |>
    Json.Decode.Pipeline.required "SetLayout" Auto.Layout.decode
    , Json.Decode.succeed SwitchLayout |>
    Json.Decode.Pipeline.required "SwitchLayout" Json.Decode.string
    , Json.Decode.succeed HideElement |>
    Json.Decode.Pipeline.required "HideElement" Json.Decode.string
    , Json.Decode.succeed ShowElement |>
    Json.Decode.Pipeline.required "ShowElement" Json.Decode.string
    , Json.Decode.succeed AddElement |>
    Json.Decode.Pipeline.required "AddElement" Auto.FullElement.decode
    , Json.Decode.succeed RemoveElement |>
    Json.Decode.Pipeline.required "RemoveElement" Json.Decode.string
    , Json.Decode.succeed SetBackgroundColour |>
    Json.Decode.Pipeline.required "SetBackgroundColour" Auto.Colour.decode
    , Json.Decode.field "SetIndicatorHollowness" (Json.Decode.succeed SetIndicatorHollowness |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.field "SetIndicatorArcStart" (Json.Decode.succeed SetIndicatorArcStart |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.field "SetIndicatorArcEnd" (Json.Decode.succeed SetIndicatorArcEnd |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.field "SetIndicatorShape" (Json.Decode.succeed SetIndicatorShape |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Auto.Shape.decode))
    , Json.Decode.field "SetIndicatorCentre" (Json.Decode.succeed SetIndicatorCentre |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Util.decodeVec2))
    , Json.Decode.field "SetIndicatorColour" (Json.Decode.succeed SetIndicatorColour |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Auto.Colour.decode))
    , Json.Decode.field "SetSliderPosition" (Json.Decode.succeed SetSliderPosition |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.float))
    , Json.Decode.field "SetButtonColour" (Json.Decode.succeed SetButtonColour |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Auto.Colour.decode))
    , Json.Decode.field "SetButtonPressed" (Json.Decode.succeed SetButtonPressed |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.bool))
    , Json.Decode.succeed ResetLayout |>
    Json.Decode.Pipeline.required "ResetLayout" Auto.ResetLayout.decode
    , Json.Decode.succeed Ping |>
    Json.Decode.Pipeline.required "Ping" Json.Decode.string ]