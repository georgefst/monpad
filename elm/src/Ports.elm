port module Ports exposing (sendUpdate)

{-| Provide safe interfaces to any ports
-}

import Auto.Update exposing (Update)
import Json.Encode as JE


sendUpdate : Update -> Cmd msg
sendUpdate =
    sendUpdatePort << Auto.Update.encode


port sendUpdatePort :
    JE.Value
    -> Cmd msg
