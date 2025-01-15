{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where

import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as J
import Data.Colour (AlphaColour, alphaChannel, black, over)
import Data.Colour.SRGB (RGB (channelBlue, channelGreen, channelRed), toSRGB)
import Deriving.Aeson (CustomJSON (CustomJSON))
import GHC.Generics (Generic)
import Linear.V2 (V2 (V2))

import Monpad.JSON

data Vec2 a = Vec2
    { x :: a
    , y :: a
    }
    deriving (Generic, ToJSON)

instance ToJSON (V2 Int) where
    toJSON = J.toJSON . \(V2 x y) -> Vec2 x y
instance ToJSON (V2 Word) where
    toJSON = J.toJSON . \(V2 x y) -> Vec2 x y
instance ToJSON (V2 Double) where
    toJSON = J.toJSON . \(V2 x y) -> Vec2 x y

deriving instance FromJSON (V2 Double)

instance ToJSON (AlphaColour Double) where
    toJSON =
        J.toJSON . \c ->
            let rgb = toSRGB $ c `over` black
             in Colour
                    { red = rgb.channelRed
                    , green = rgb.channelGreen
                    , blue = rgb.channelBlue
                    , alpha = alphaChannel c
                    }

data Colour = Colour
    { red :: Double
    , green :: Double
    , blue :: Double
    , alpha :: Double
    }
    deriving (Show, Generic)
    deriving (ToJSON) via CustomJSON JSON Colour
