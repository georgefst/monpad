{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances for working with JSON and Dhall.
module Orphans.V2 () where

import Data.Aeson (ToJSON)
import Dhall (FromDhall (autoWith))
import GHC.Generics (Generic)
import Linear.V2 (V2 (V2))

data Vec2 a = Vec2
    { x :: a
    , y :: a
    }
    deriving (Generic, FromDhall, ToJSON)

instance FromDhall (V2 Int) where
    autoWith = fmap (\(Vec2 x y) -> V2 x y) . autoWith
instance FromDhall (V2 Word) where
    autoWith = fmap (\(Vec2 x y) -> V2 x y) . autoWith
instance FromDhall (V2 Double) where
    autoWith = fmap (\(Vec2 x y) -> V2 x y) . autoWith
