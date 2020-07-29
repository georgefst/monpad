module Util where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Type.Reflection (Typeable, typeRep)

symbolValT :: forall a. KnownSymbol a => Text
symbolValT = T.pack $ symbolVal $ Proxy @a

showT :: Show a => a -> Text
showT = T.pack . show

typeRepT :: forall a. Typeable a => Text
typeRepT = showT $ typeRep @a

biVoid :: Bifunctor p => p a b -> p () ()
biVoid = bimap (const ()) (const ())

untilLeft :: Monad m => m (Either e a) -> m e
untilLeft x = x >>= either pure (const $ untilLeft x)

mapRightM :: Monad m => (a -> m b) -> Either e a -> m (Either e b)
mapRightM f = either (return . Left) (fmap Right . f)

-- | Like 'listDirectory', but returns paths relative to the input.
listDirectory' :: FilePath -> IO [FilePath]
listDirectory' d = map (d </>) <$> listDirectory d
