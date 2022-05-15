module Binary exposing (..)

import Auto.ClientUpdate exposing (ClientUpdate(..))
import Bytes exposing (..)
import Bytes.Encode exposing (..)
import Math.Vector2 as Vec2
import Murmur3


hashElementID : String -> Int
hashElementID =
    Murmur3.hashString 0


clientUpdateEncoder : ClientUpdate -> Encoder
clientUpdateEncoder u =
    case u of
        ButtonUp i ->
            sequence
                [ unsignedInt8 0
                , unsignedInt32 LE <| hashElementID i
                ]

        ButtonDown i ->
            sequence
                [ unsignedInt8 1
                , unsignedInt32 LE <| hashElementID i
                ]

        StickMove i x ->
            sequence
                [ unsignedInt8 2
                , unsignedInt32 LE <| hashElementID i
                , float64 LE <| Vec2.getX x
                , float64 LE <| Vec2.getY x
                ]

        SliderMove i x ->
            sequence
                [ unsignedInt8 3
                , unsignedInt32 LE <| hashElementID i
                , float64 LE x
                ]

        InputBool i x ->
            sequence
                [ unsignedInt8 4
                , unsignedInt32 LE <| hashElementID i
                , unsignedInt8
                    (if x then
                        1

                     else
                        0
                    )
                ]

        InputNumber i x ->
            sequence
                [ unsignedInt8 5
                , unsignedInt32 LE <| hashElementID i
                , signedInt32 LE x
                ]

        InputText i x ->
            sequence
                [ unsignedInt8 6
                , unsignedInt32 LE <| hashElementID i
                , string x
                ]

        SubmitInput i ->
            sequence
                [ unsignedInt8 7
                , unsignedInt32 LE <| hashElementID i
                ]

        Pong x ->
            sequence
                [ unsignedInt8 8
                , string x
                ]
