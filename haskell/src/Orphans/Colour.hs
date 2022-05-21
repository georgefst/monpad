{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Colour () where

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson qualified as J
import Data.Colour (AlphaColour, alphaChannel, black, over, withOpacity)
import Data.Colour.SRGB (RGB (channelBlue, channelGreen, channelRed), sRGB, toSRGB)
import Deriving.Aeson (CustomJSON (CustomJSON))
import Dhall (FromDhall (autoWith), Generic)
import GenerateElm.Via qualified as Elm
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder (elmDecoderDefinition), HasElmType (elmDefinition))
import Opts qualified

instance ToJSON (AlphaColour Double) where
    toJSON =
        J.toJSON . \c ->
            let rgb = toSRGB $ c `over` black
             in Colour'
                    { red = rgb.channelRed
                    , green = rgb.channelGreen
                    , blue = rgb.channelBlue
                    , alpha = alphaChannel c
                    }

instance FromDhall (AlphaColour Double) where
    autoWith = fmap (\(c :: Colour') -> withOpacity (sRGB c.red c.green c.blue) c.alpha) . autoWith

instance HasElmDecoder J.Value (AlphaColour Double) where
    elmDecoderDefinition = elmDecoderDefinition @J.Value @Colour'
instance HasElmType (AlphaColour Double) where
    elmDefinition = elmDefinition @Colour'

data Colour' = Colour'
    { red :: Double
    , green :: Double
    , blue :: Double
    , alpha :: Double
    }
    deriving (Show, Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (ToJSON) via CustomJSON Opts.JSON Colour'
    deriving (HasElmType, HasElmDecoder J.Value) via Elm.Via Colour'
