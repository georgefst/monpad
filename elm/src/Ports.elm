port module Ports exposing (receiveUpdates, sendUpdate)

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


receiveUpdates : Sub (Maybe (List ServerUpdate))
receiveUpdates =
    receiveUpdatePort <|
        \val ->
            case JD.decodeValue decodeUpdates val of
                Ok v ->
                    Just v

                Err _ ->
                    Nothing


decodeUpdates : JD.Decoder (List ServerUpdate)
decodeUpdates =
    JD.oneOf
        [ Auto.ServerUpdate.decode |> JD.map List.singleton
        , JD.list Auto.ServerUpdate.decode
        ]


port receiveUpdatePort :
    (JD.Value -> msg)
    -> Sub msg
