port module Ports exposing (receiveUpdate, sendUpdate)

{-| Provide safe interfaces to any ports
-}

import Auto.ServerUpdate exposing (ServerUpdate)
import Auto.Update exposing (Update)
import Json.Decode as JD
import Json.Encode as JE


sendUpdate : Update -> Cmd msg
sendUpdate =
    sendUpdatePort << Auto.Update.encode


port sendUpdatePort :
    JE.Value
    -> Cmd msg


receiveUpdate : Sub (Maybe ServerUpdate)
receiveUpdate =
    receiveUpdatePort <|
        \val ->
            case JD.decodeValue Auto.ServerUpdate.decode val of
                Ok v ->
                    Just v

                Err e ->
                    Nothing


port receiveUpdatePort :
    (JD.Value -> msg)
    -> Sub msg
