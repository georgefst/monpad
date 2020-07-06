module Util.IntVector2 exposing (..)

import Json.Decode
import Json.Decode.Pipeline


type alias IntVector2 =
    { x : Int
    , y : Int
    }

decode : Json.Decode.Decoder IntVector2
decode =
    Json.Decode.succeed IntVector2
        |> Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.int)
        |> Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.int)


unVec : IntVector2 -> ( Float, Float )
unVec v =
    ( Basics.toFloat v.x, Basics.toFloat v.y )
