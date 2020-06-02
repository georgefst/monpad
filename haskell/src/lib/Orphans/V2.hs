{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans.V2 where

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as J
import Generics.SOP qualified as SOP
import Language.Elm.Expression qualified as Expr
import Language.Elm.Name qualified as Name
import Language.Elm.Type qualified as Type
import Language.Haskell.To.Elm
import Linear.V2 (V2)

deriving instance ToJSON (V2 Double)
deriving instance FromJSON (V2 Double)

--TODO we could presumably derive the elm encoder
    --  would require a slight module reorganization
-- link with Elm's 'Vec2'
deriving instance SOP.Generic (V2 Double)
deriving instance SOP.HasDatatypeInfo (V2 Double)
instance HasElmEncoder J.Value (V2 Double) where
    elmEncoder = Expr.Global $ Name.Qualified ["Util"] "encodeVec2"
instance HasElmType (V2 Double) where
    elmType = Type.Global $ Name.Qualified ["Math", "Vector2"] "Vec2"
