{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Servant.Colour () where

import Data.Bifunctor (first)
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24reads)
import Data.Text qualified as T
import Servant (FromHttpApiData (parseUrlPiece))
import Text.Read (readEither)

instance FromHttpApiData (Colour Float) where
    parseUrlPiece = first T.pack . readEither' . T.unpack

{- TODO there should be a better way
https://www.reddit.com/r/haskell/comments/ufrk6a/comment/i9c4sg9/?utm_source=share&utm_medium=web2x&context=3
-}
readEither' :: String -> Either String (Colour Float)
readEither' = fmap (\(ReadWrapper c) -> c) . readEither @ReadWrapper
newtype ReadWrapper = ReadWrapper (Colour Float)
instance Read ReadWrapper where
    readsPrec _ = map (first ReadWrapper) . sRGB24reads @Float
