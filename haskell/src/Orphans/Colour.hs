{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Colour () where

import Data.Bifunctor (first)
import Data.Colour (AlphaColour, withOpacity)
import Data.Colour qualified as Colour
import Data.Colour.SRGB (sRGB, sRGB24reads)
import Data.Text qualified as T
import Dhall (FromDhall (autoWith), Generic)
import Servant (FromHttpApiData (parseUrlPiece))
import Text.Read (readEither)

instance FromDhall (AlphaColour Double) where
    autoWith = fmap (\(c :: Colour) -> withOpacity (sRGB c.red c.green c.blue) c.alpha) . autoWith

data Colour = Colour
    { red :: Double
    , green :: Double
    , blue :: Double
    , alpha :: Double
    }
    deriving (Show, Generic, FromDhall)

instance FromHttpApiData (Colour.Colour Float) where
    parseUrlPiece = first T.pack . readEither' . T.unpack

{- TODO there should be a better way
https://www.reddit.com/r/haskell/comments/ufrk6a/comment/i9c4sg9/?utm_source=share&utm_medium=web2x&context=3
-}
readEither' :: String -> Either String (Colour.Colour Float)
readEither' = fmap (\(ReadWrapper c) -> c) . readEither @ReadWrapper
newtype ReadWrapper = ReadWrapper (Colour.Colour Float)
instance Read ReadWrapper where
    readsPrec _ = map (first ReadWrapper) . sRGB24reads @Float
