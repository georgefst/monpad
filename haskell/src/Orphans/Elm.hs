{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Elm where

import Data.Aeson qualified as J
import Data.List.NonEmpty
import Language.Haskell.To.Elm

instance HasElmType a => HasElmType (NonEmpty a) where
    elmType = elmType @[a]
instance HasElmDecoder J.Value a => HasElmDecoder J.Value (NonEmpty a) where
    elmDecoder = elmDecoder @J.Value @[a]
