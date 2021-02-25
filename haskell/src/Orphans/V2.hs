{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances for working with Elm and Dhall.
module Orphans.V2 () where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Dhall (FromDhall (autoWith))
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Language.Elm.Expression qualified as Expr
import Language.Elm.Name qualified as Name
import Language.Elm.Type qualified as Type
import Language.Haskell.To.Elm (
    HasElmDecoder (..),
    HasElmEncoder (..),
    HasElmType (..),
 )
import Linear.V2 (V2 (V2))
import Util.Elm qualified as Elm

-- NB we can decode but encoding would be unsafe
instance HasElmType Word where
    elmType = "Basics.Int"
instance HasElmDecoder J.Value Word where
    elmDecoder = "Json.Decode.int"

data IntVec2 = IntVec2
    { x :: Int
    , y :: Int
    }
    deriving (Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo, ToJSON)
    deriving (HasElmType, HasElmDecoder J.Value) via Elm.Via IntVec2
data WordVec2 = WordVec2
    { x :: Word
    , y :: Word
    }
    deriving (Generic, FromDhall, SOP.Generic, SOP.HasDatatypeInfo, ToJSON)
    deriving (HasElmType, HasElmDecoder J.Value) via Elm.Via IntVec2

instance ToJSON (V2 Int) where
    toJSON = J.toJSON . \(V2 x y) -> IntVec2 x y
instance ToJSON (V2 Word) where
    toJSON = J.toJSON . \(V2 x y) -> WordVec2 x y

instance FromDhall (V2 Int) where
    autoWith = fmap (\(IntVec2 x y) -> V2 x y) . autoWith
instance FromDhall (V2 Word) where
    autoWith = fmap (\(WordVec2 x y) -> V2 x y) . autoWith

deriving instance SOP.Generic (V2 Int)
deriving instance SOP.HasDatatypeInfo (V2 Int)
instance HasElmDecoder J.Value (V2 Int) where
    elmDecoderDefinition = elmDecoderDefinition @J.Value @IntVec2
instance HasElmType (V2 Int) where
    elmDefinition = elmDefinition @IntVec2

deriving instance SOP.Generic (V2 Word)
deriving instance SOP.HasDatatypeInfo (V2 Word)
instance HasElmDecoder J.Value (V2 Word) where
    elmDecoderDefinition = elmDecoderDefinition @J.Value @WordVec2
instance HasElmType (V2 Word) where
    elmDefinition = elmDefinition @WordVec2

deriving instance ToJSON (V2 Double)
deriving instance FromJSON (V2 Double)
deriving instance SOP.Generic (V2 Double)
deriving instance SOP.HasDatatypeInfo (V2 Double)
instance HasElmEncoder J.Value (V2 Double) where
    elmEncoder = Expr.Global $ Name.Qualified ["Util"] "encodeVec2"
instance HasElmType (V2 Double) where
    elmType = Type.Global $ Name.Qualified ["Math", "Vector2"] "Vec2"
