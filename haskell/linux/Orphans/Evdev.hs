{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Evdev where

import Dhall (FromDhall)
import Evdev.Codes (AbsoluteAxis (..), Key (..), RelativeAxis (..))
import GHC.Generics (Generic)

deriving instance Generic Key
deriving instance FromDhall Key
deriving instance Generic AbsoluteAxis
deriving instance FromDhall AbsoluteAxis
deriving instance Generic RelativeAxis
deriving instance FromDhall RelativeAxis
