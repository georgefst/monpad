port module Ports exposing (consoleLog, receiveUpdates, sendUpdate, toggleFullscreen)

{-| Provide safe interfaces to any ports
-}

import Auto.ServerUpdate exposing (ServerUpdate)
import Auto.Update exposing (Update)
import Json.Decode as JD
import Json.Encode as JE


toggleFullscreen : Cmd msg
toggleFullscreen =
    fullscreenPort ()


port fullscreenPort : () -> Cmd msg


sendUpdate : Update -> Cmd msg
sendUpdate =
    sendUpdatePort << Auto.Update.encode


port sendUpdatePort :
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
