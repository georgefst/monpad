module Util where

import Control.Concurrent
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Data.Time
import Streamly
import System.FilePath
import Util.Util

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Dhall.Core qualified as D
import Dhall.Core qualified as Dhall
import Dhall.Import qualified as D
import Dhall.Parser qualified as Dhall
import Network.HostName (getHostName)
import Network.Socket (
    AddrInfo (addrAddress),
    HostAddress,
    HostName,
    SockAddr (SockAddrInet),
    getAddrInfo,
    hostAddressToTuple,
 )
import Streamly.Prelude qualified as SP

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

-- | Attach an extra action to each element of the stream.
traceStream :: (a -> IO ()) -> Serial a -> Serial a
traceStream f = SP.mapM \x -> f x >> pure x

-- | Ignore events which are followed within a given number of milliseconds.
lastOfGroup :: Int -> Async a -> Serial a
lastOfGroup interval = f2 . asyncly . f1
  where
    -- delay everything by `interval`, and insert 'Nothing' markers where the value first came in
    f1 = SP.concatMapWith (<>) \x ->
        SP.yieldM (pure Nothing) <> SP.yieldM (threadDelay interval >> pure (Just x))
    -- ignore any event which appears within `interval` of a 'Nothing'
    f2 = SP.mapMaybe id . SP.map snd . flip SP.postscanlM' (const False, undefined) \(tooSoon, _) -> \case
        Just x -> do
            t <- getCurrentTime
            pure (tooSoon, guard (not $ tooSoon t) $> x)
        Nothing -> do
            t <- getCurrentTime
            pure (\t' -> diffUTCTime t' t < realToFrac interval, Nothing)

-- | Returns list of files (transitively) imported by the expression, grouped by directory.
dhallImports :: MonadIO io => Text -> io [(FilePath, NonEmpty Text)]
dhallImports t = do
    e <- Dhall.throws $ Dhall.exprFromText "" t
    --TODO we could return and reuse the ignored expression, rather than re-running the previous step in the outer scope
    (_e', s) <- liftIO $ flip runStateT (D.emptyStatus $ dropFileName "") $ D.loadWith e
    pure $ classifyOnFst $ nubOrd $ mapMaybe (importFile . D.chainedImport . D.child) $ D._graph s
  where
    importFile x = case D.importType $ D.importHashed x of
        D.Local _ file -> Just (joinPath $ reverse $ map T.unpack $ D.components $ D.directory file, D.file file)
        _ -> Nothing
