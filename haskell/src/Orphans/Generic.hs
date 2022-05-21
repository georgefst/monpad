{-# OPTIONS_GHC -Wno-orphans #-}

-- | These are mostly/entirely for the sake of using `generic-optics`.
module Orphans.Generic () where

import GHC.Generics (Generic)
import Network.WebSockets.Connection qualified as WS

deriving instance Generic WS.PendingConnection
deriving instance Generic WS.ConnectionOptions
