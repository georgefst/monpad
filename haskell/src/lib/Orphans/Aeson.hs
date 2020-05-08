{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Aeson where

import Data.Aeson (ToJSON, FromJSON)
import Linear.V2 (V2)

deriving instance ToJSON (V2 Double)
deriving instance FromJSON (V2 Double)
