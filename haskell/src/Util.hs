module Util where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Type.Reflection (Typeable, typeRep)

symbolValT :: forall a. KnownSymbol a => Text
symbolValT = T.pack $ symbolVal $ Proxy @a

showT :: Show a => a -> Text
showT = T.pack . show

typeRepT :: forall a. Typeable a => Text
typeRepT = showT $ typeRep @a
