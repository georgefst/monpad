module Util where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HostName (getHostName)
import Network.Socket (
    AddrInfo (addrAddress),
    HostAddress,
    HostName,
    SockAddr (SockAddrInet),
    getAddrInfo,
    hostAddressToTuple,
 )
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Type.Reflection (Typeable, typeRep)

symbolValT :: forall a. KnownSymbol a => Text
symbolValT = T.pack $ symbolVal $ Proxy @a

showT :: Show a => a -> Text
showT = T.pack . show

typeRepT :: forall a. Typeable a => Text
typeRepT = showT $ typeRep @a

infixl 4 <<$>>
(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

biVoid :: Bifunctor p => p a b -> p () ()
biVoid = bimap (const ()) (const ())

untilLeft :: Monad m => m (Either e a) -> m e
untilLeft x = x >>= either pure (const $ untilLeft x)

mapRightM :: Monad m => (a -> m b) -> Either e a -> m (Either e b)
mapRightM f = either (return . Left) (fmap Right . f)

-- | Like 'listDirectory', but returns paths relative to the input.
listDirectory' :: FilePath -> IO [FilePath]
listDirectory' d = map (d </>) <$> listDirectory d

getLocalIp :: IO (Maybe HostAddress)
getLocalIp = do
    h <- getHostName'
    sockAddrs <- map addrAddress <$> getAddrInfo Nothing (Just $ h <> ".local") Nothing
    pure . find bitOfAHack $ flip mapMaybe sockAddrs \case
        SockAddrInet _ a -> Just a
        _ -> Nothing
  where
    --TODO
    bitOfAHack = (== 192) . fst4 . hostAddressToTuple

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

-- adapted from an internal function of the same name in Network.Socket.Info
showHostAddress :: HostAddress -> Text
showHostAddress ip =
    let (u3, u2, u1, u0) = hostAddressToTuple ip
     in T.intercalate "." $ map showT [u3, u2, u1, u0]

--TODO if maintainer doesn't respond to my email fixing this, fork
getHostName' :: IO HostName
getHostName' = f <$> getHostName
  where
    f x = maybe x T.unpack $ T.stripSuffix ".local" $ T.pack x
