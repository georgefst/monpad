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

import Auto.ClientUpdate exposing (ClientUpdate)
import Auto.Encoding exposing (Encoding(..))
import Auto.ServerUpdate exposing (ServerUpdate)
import Binary
import Bytes.Encode as BE
import Json.Decode as JD
import Json.Encode as JE
import Util.EncodeBytes


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


sendUpdate : Encoding -> ClientUpdate -> Cmd msg
sendUpdate enc =
    case enc of
        JSONEncoding ->
            sendUpdatePortJSON << Auto.ClientUpdate.encode

        BinaryEncoding ->
            sendUpdatePortBinary << Util.EncodeBytes.bytesToList << BE.encode << Binary.clientUpdateEncoder


port sendUpdatePortBinary :
    List Int
    -> Cmd msg


port sendUpdatePortJSON :
    JE.Value
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
