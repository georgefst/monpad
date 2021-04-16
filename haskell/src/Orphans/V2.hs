{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances for working with JSON and Dhall.
module Orphans.V2 () where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Dhall (FromDhall (autoWith))
import GHC.Generics (Generic)
import Linear.V2 (V2 (V2))

data IntVec2 = IntVec2
    { x :: Int
    , y :: Int
    }
    deriving (Generic, FromDhall, ToJSON)
data WordVec2 = WordVec2
    { x :: Word
    , y :: Word
    }
    deriving (Generic, FromDhall, ToJSON)
data DoubleVec2 = DoubleVec2
    { x :: Double
    , y :: Double
    }
    deriving (Generic, FromDhall, ToJSON)

instance ToJSON (V2 Int) where
    toJSON = J.toJSON . \(V2 x y) -> IntVec2 x y
instance ToJSON (V2 Word) where
    toJSON = J.toJSON . \(V2 x y) -> WordVec2 x y
instance ToJSON (V2 Double) where
    toJSON = J.toJSON . \(V2 x y) -> DoubleVec2 x y

instance FromDhall (V2 Int) where
    autoWith = fmap (\(IntVec2 x y) -> V2 x y) . autoWith
instance FromDhall (V2 Word) where
    autoWith = fmap (\(WordVec2 x y) -> V2 x y) . autoWith
instance FromDhall (V2 Double) where
    autoWith = fmap (\(DoubleVec2 x y) -> V2 x y) . autoWith

deriving instance FromJSON (V2 Double)
