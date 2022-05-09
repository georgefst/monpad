port module Ports exposing
    ( consoleLog
    , fullscreenChanges
    , goFullscreen
    , playAudio
    , receiveUpdates
    , resetForm
    , sendInit
    , sendUpdate
    , vibrate
    )

{-| Provide safe interfaces to any ports
-}

import Auto.ClientUpdate exposing (ClientUpdate(..))
import Auto.ServerUpdate exposing (ServerUpdate)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BDecode
import Bytes.Encode as Encode exposing (Encoder)
import Json.Decode as JD
import List exposing (reverse)
import Math.Vector2 exposing (getX, getY)
import Murmur3


goFullscreen : Cmd msg
goFullscreen =
    fullscreenPort ()


port fullscreenPort : () -> Cmd msg


fullscreenChanges : Sub (Result JD.Error Bool)
fullscreenChanges =
    fullscreenChangePort <|
        JD.decodeValue JD.bool


port fullscreenChangePort :
    (JD.Value -> msg)
    -> Sub msg



--TODO move a lot of the new stuff out of this module


sendUpdate : ClientUpdate -> Cmd msg
sendUpdate =
    sendUpdatePort << decodeImage << Encode.encode << clientUpdateEncoder



--TODO cache this?


hashElementID : String -> Int
hashElementID =
    Murmur3.hashString 0


clientUpdateEncoder : ClientUpdate -> Encoder
clientUpdateEncoder u =
    case u of
        ButtonUp i ->
            Encode.sequence
                [ Encode.unsignedInt8 0
                , Encode.unsignedInt32 LE <| hashElementID i
                ]

        ButtonDown i ->
            Encode.sequence
                [ Encode.unsignedInt8 1
                , Encode.unsignedInt32 LE <| hashElementID i
                ]

        StickMove i x ->
            Encode.sequence
                [ Encode.unsignedInt8 2
                , Encode.unsignedInt32 LE <| hashElementID i
                , Encode.float64 LE <| getX x
                , Encode.float64 LE <| getY x
                ]

        SliderMove i x ->
            Encode.sequence
                [ Encode.unsignedInt8 3
                , Encode.unsignedInt32 LE <| hashElementID i
                , Encode.float64 LE x
                ]

        InputBool i x ->
            Encode.sequence
                [ Encode.unsignedInt8 4
                , Encode.unsignedInt32 LE <| hashElementID i
                , Encode.unsignedInt8
                    (if x then
                        1

                     else
                        0
                    )
                ]

        InputNumber i x ->
            Encode.sequence
                [ Encode.unsignedInt8 5
                , Encode.unsignedInt32 LE <| hashElementID i
                , Encode.float64 LE x
                ]

        InputText i x ->
            Encode.sequence
                [ Encode.unsignedInt8 6
                , Encode.unsignedInt32 LE <| hashElementID i
                , Encode.string x
                ]

        SubmitInput i ->
            Encode.sequence
                [ Encode.unsignedInt8 7
                , Encode.unsignedInt32 LE <| hashElementID i
                ]

        Pong x ->
            Encode.sequence
                [ Encode.unsignedInt8 8
                , Encode.string x
                ]


port sendUpdatePort :
    List Int
    -> Cmd msg


receiveUpdates : Sub (Result JD.Error (List ServerUpdate))
receiveUpdates =
    receiveUpdatePort <|
        JD.decodeValue decodeUpdates


decodeUpdates : JD.Decoder (List ServerUpdate)
decodeUpdates =
    JD.oneOf
        [ Auto.ServerUpdate.decode |> JD.map List.singleton
        , JD.list Auto.ServerUpdate.decode
        ]


port receiveUpdatePort :
    (JD.Value -> msg)
    -> Sub msg


consoleLog : String -> Cmd msg
consoleLog =
    logPort


port logPort :
    String
    -> Cmd msg


sendInit : Cmd msg
sendInit =
    initPort ()


port initPort :
    ()
    -> Cmd msg


playAudio : String -> Cmd msg
playAudio =
    audioPort


port audioPort :
    String
    -> Cmd msg


vibrate : List Int -> Cmd msg
vibrate =
    vibratePort


port vibratePort :
    List Int
    -> Cmd msg


resetForm : String -> Cmd msg
resetForm =
    resetFormPort


port resetFormPort : String -> Cmd msg



--TODO move to separate module, with explanation (Elm doesn't allow Bytes to be sent through a port directly)
--TODO https://discourse.elm-lang.org/t/bytes-ports-and-uint8array/3569/2


decodeImage : Bytes -> List Int
decodeImage encodedImage =
    let
        listDecode =
            bytesListDecode BDecode.unsignedInt8 (Bytes.width encodedImage)
    in
    --TODO this `reverse` isn't in the original Discourse code
    reverse <| Maybe.withDefault [] (BDecode.decode listDecode encodedImage)


bytesListDecode : BDecode.Decoder a -> Int -> BDecode.Decoder (List a)
bytesListDecode decoder len =
    BDecode.loop ( len, [] ) (listStep decoder)


listStep : BDecode.Decoder a -> ( Int, List a ) -> BDecode.Decoder (BDecode.Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        BDecode.succeed (BDecode.Done xs)

    else
        BDecode.map (\x -> BDecode.Loop ( n - 1, x :: xs )) decoder
