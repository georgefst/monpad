{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans.V2 () where

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as J
import Dhall (FromDhall(autoWith))
import Generics.SOP qualified as SOP
import GHC.Generics (Generic)
import Language.Elm.Expression qualified as Expr
import Language.Elm.Name qualified as Name
import Language.Elm.Type qualified as Type
import Language.Haskell.To.Elm
import Linear.V2 (V2(V2))
import Numeric.Natural (Natural)

deriving instance ToJSON (V2 Double)
deriving instance FromJSON (V2 Double)
deriving instance ToJSON (V2 Int)
deriving instance FromJSON (V2 Int)

--TODO we could presumably derive the elm encoder
    --  would require a slight module reorganization
deriving instance SOP.Generic (V2 Double)
deriving instance SOP.HasDatatypeInfo (V2 Double)
instance HasElmEncoder J.Value (V2 Double) where
    elmEncoder = Expr.Global $ Name.Qualified ["Util"] "encodeVec2"
instance HasElmType (V2 Double) where
    elmType = Type.Global $ Name.Qualified ["Math", "Vector2"] "Vec2"

deriving instance SOP.Generic (V2 Int)
deriving instance SOP.HasDatatypeInfo (V2 Int)
instance HasElmDecoder J.Value (V2 Int) where
    elmDecoder = Expr.Global $ Name.Qualified ["Util", "IntVector2"] "decode"
instance HasElmType (V2 Int) where
    elmType = Type.Global $ Name.Qualified ["Util", "IntVector2"] "IntVector2"

data V = V
    { x :: Int
    , y :: Int
    }
    deriving (Generic, FromDhall)
instance FromDhall (V2 Int) where
    autoWith = fmap (\(V x y) -> V2 x y) . autoWith
instance FromDhall Int where -- obviously this conversion isn't fully safe
    autoWith = fmap (fromIntegral @Natural) . autoWith
