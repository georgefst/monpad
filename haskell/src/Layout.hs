{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Layout where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Dhall (FromDhall)
import Orphans.V2 ()

import Monpad.Core
import Orphans.Colour ()
import Util

-- | A (non-empty) list of 'Layout's.
type Layouts a b = NonEmpty (Layout a b, DhallExpr)
layoutsFromDhall :: (FromDhall a, FromDhall b) => Logger -> NonEmpty Text -> IO (Maybe (Layouts a b))
layoutsFromDhall write = runMaybeT . traverse \t -> do
    e <- dhallExprFromText write t
    (l, _) <- dhallToHs write e
    pure (l, e)

deriving newtype instance FromDhall (LayoutID)
deriving instance (FromDhall a, FromDhall b) => FromDhall (Layout a b)
deriving instance (FromDhall a, FromDhall b) => FromDhall (FullElement a b)
deriving newtype instance FromDhall ElementID
deriving instance (FromDhall a, FromDhall b) => FromDhall (Element a b)
deriving instance (FromDhall a) => FromDhall (Stick a)
deriving instance (FromDhall b) => FromDhall (Button b)
deriving instance (FromDhall a) => FromDhall (Slider a)
deriving instance FromDhall (Input)
deriving instance FromDhall (InputType)
deriving instance FromDhall (NumberInput)
deriving instance FromDhall (TextInput)
deriving instance FromDhall (Image)
deriving instance FromDhall (PosX)
deriving instance FromDhall (PosY)
deriving instance FromDhall (TextBox)
deriving instance FromDhall (Indicator)
deriving instance FromDhall (Shape)
deriving instance FromDhall (ViewBox)
deriving instance FromDhall (TextStyle)
deriving instance FromDhall (TextShadow)
