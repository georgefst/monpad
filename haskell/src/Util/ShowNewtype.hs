module Util.ShowNewtype where

import Data.Proxy
import GHC.TypeLits

--TODO use Generic - would be cool, and should allow us to avoid needing to pass the type name

-- | Allows us to create less noisy 'Show' instances for newtypes with a labelled field.
newtype ShowNewtypeWithoutRecord (constructor :: Symbol) a = ShowNewtypeWithoutRecord a

instance (KnownSymbol constructor, Show a) => Show (ShowNewtypeWithoutRecord constructor a) where
    showsPrec d (ShowNewtypeWithoutRecord x) =
        showParen (d >= 11) (showString (symbolVal (Proxy @constructor) ++ " ") . showsPrec 11 x)
